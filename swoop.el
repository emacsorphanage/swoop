;;; TODO
;; Unpropertize (thing-at-point 'symbol)
;; Eliminate flickering update effect
;; Minibuffer history


;;; Code
(require 'dash)
(require 'hi-lock)
(require 'ht)
(require 'async)
;; (my-nest-specific-text-property-position 'invisible 'swoop)
;; buffer-invisibility-spec

(defgroup swoop nil
  "Group for swoop"
  :prefix "swoop-" :group 'convenience)

(defvar swoop-buffer "*Swoop*")
(defvar swoop-window nil)
(defvar swoop-target-buffer nil)
(defvar swoop-target-window nil)
(defvar swoop-buffer-selection-overlay nil)
(defvar swoop-target-buffer-selection-overlay nil)

(defvar swoop--last-position 1)
(defvar swoop--cached-count 0)
(defvar swoop--minibuf-last-content "")
(defvar swoop--last-query-plain "")
(defvar swoop--last-query-converted "")
(defvar swoop-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "C-n") 'swoop--next-line)
    (define-key map (kbd "C-p") 'swoop--prev-line)
    (define-key map (kbd "C-g") 'swoop--cancel)
    (define-key map (kbd "RET") 'swoop--default-action)
    (define-key map (kbd "<C-return>") 'swoop--default-action)
    map))

;; Option
(setq swoop-font-size-change: 0.9)
(setq swoop-use-target-magnifier: t)


;; Cancel action
(defvar swoop-abort-hook nil)
(defun swoop--cancel ()
  "This is assigned to `C-g' as default. Exit from Swoop (minibuffer),
and execute functions listed in swoop-abort-hook"
  (interactive)
  (run-with-timer
   0 nil (lambda () (run-hooks 'swoop-abort-hook)))
  (exit-minibuffer))
(defun swoop-back-to-last-position ()
  (interactive)
  (let (($po swoop--last-position))
    (setq swoop--last-position (point))
    (with-selected-window swoop-target-window
      (goto-char $po))))
(defun swoop--highlight-for-cancel ()
  (interactive)
  (let* (($lbeg (point))
         ($lend (line-end-position))
         ($lov (make-overlay $lbeg $lend))
         ($lbeg2 (line-beginning-position))
         ($lend2 $lbeg)
         ($lov2 (make-overlay $lbeg2 $lend2)))
    (run-with-timer 0.3 nil (lambda ($o) (delete-overlay $o)) $lov)
    (overlay-put $lov 'face 'swoop-target-words-face)
    (run-with-timer 0.3 nil (lambda ($o) (delete-overlay $o)) $lov2)
    (overlay-put $lov2 'face 'swoop-target-line-face)))
(add-hook 'swoop-abort-hook 'swoop-back-to-last-position)
(add-hook 'swoop-abort-hook 'swoop--highlight-for-cancel)

(defface swoop-target-line-face
  '((t :background "#e3e300" :foreground "#222222"))
  "Target line face for swoop"
  :group 'swoop)
(defface swoop-target-words-face
  '((t :background "#7700ff" :foreground "#ffffff"))
  "Target words face for swoop"
  :group 'swoop)
(defface swoop-line-number-face
  '((t :foreground "#00eeff"))
  "Line number face for swoop"
  :group 'swoop)

(defmacro swoop--mapc ($variable $list &rest $body)
  "Same as `mapc'"
  (declare (indent 2))
  (let (($list-unique (cl-gensym)))
    `(let ((,$list-unique ,$list))
       (mapc (lambda (,$variable)
               ,@$body)
             ,$list-unique))))
(defmacro swoop--mapcr ($variable $list &rest $body)
  "Same as `mapcar'"
  (declare (indent 2))
  (let (($list-unique (cl-gensym)))
    `(let ((,$list-unique ,$list)
           ($results))
       (mapc (lambda (,$variable)
               (setq $results (cons (progn ,@$body) $results)))
             ,$list-unique)
       $results)))
;; (let ($c)
;;   (swoop--mapc $a '(1 2 3)
;;     (setq $c (cons (+ $a $a) $c)))
;;   $c) ;;=> (6 4 2)
;; (swoop--mapcr $a '(1 2 3) (* $a $a)) ;;=> (6 4 2)

(defun swoop--default-action () (interactive)
  (run-with-timer
   0 nil
   (lambda ($po)
     (with-selected-window swoop-target-window
       (goto-char $po)
       (save-excursion
         (re-search-forward
          (concat
           "\\("
           (mapconcat 'identity swoop--last-query-converted "\\|")
           "\\)")
          nil t))
       (goto-char (match-beginning 0))
       ;; Highlight for few seconds after jump
       (let* (($lbeg (line-beginning-position))
              ($lend (line-end-position))
              ($lov (make-overlay $lbeg $lend))
              ($wbeg (match-beginning 0))
              ($wend (match-end 0))
              ($wov (make-overlay $wbeg $wend)))
         (run-with-timer 0.28 nil (lambda ($o) (delete-overlay $o)) $lov)
         (overlay-put $lov 'face 'swoop-target-words-face)
         (run-with-timer 0.4 nil (lambda ($o) (delete-overlay $o)) $wov)
         (overlay-put $wov 'face 'swoop-target-line-face)
         (recenter))))
   (with-current-buffer swoop-target-buffer (point)))
  (exit-minibuffer))

(defvar swoop-window-split-current-window nil)
;; 'split-window-horizontally 'split-window-vertically
(defvar swoop-window-split-direction 'split-window-vertically)
(defvar swoop-display-function
  (lambda ($buf)
    (if swoop-window-split-current-window
        (funcall swoop-window-split-direction)
      (when (one-window-p)
        (funcall swoop-window-split-direction)))
    (other-window 1)
    (switch-to-buffer $buf)))

(defvar swoop-invisible-targets nil)
(defsubst swoop--restore-unveiled-overlay ()
  (when swoop-invisible-targets
    (swoop--mapc $ov swoop-invisible-targets
      (overlay-put (car $ov) 'invisible (cdr $ov)))
    (setq swoop-invisible-targets nil)))
(defsubst swoop--unveil-invisible-overlay ()
  "Show hidden text temporarily to view it during swoop.
This function needs to call after latest swoop-target-buffer-selection-overlay moved."
  (swoop--restore-unveiled-overlay)
  (swoop--mapc $ov
      (overlays-in (overlay-start swoop-target-buffer-selection-overlay)
                   (overlay-end swoop-target-buffer-selection-overlay))
    (let (($type (overlay-get $ov 'invisible)))
      (when $type
        (overlay-put $ov 'invisible nil)
        (setq swoop-invisible-targets
              (cons (cons $ov $type) swoop-invisible-targets))))))
(defsubst swoop--goto-line ($line)
  (goto-char (point-min))
  (unless (re-search-forward "\n" nil t (1- $line))
    (goto-char (point-max))))

(setq swoop-magnify-around-target-overlay nil)
(cl-defun swoop--magnify-around-target (&key ($around 10) ($size 1.2) $delete)
  (with-selected-window swoop-target-window
    (cond ((not swoop-magnify-around-target-overlay)
           (setq swoop-magnify-around-target-overlay
                 (make-overlay
                  (line-beginning-position (- 0 $around))
                  (line-beginning-position $around)))
           (overlay-put swoop-magnify-around-target-overlay
                        'face `(:height ,$size))
           (overlay-put swoop-magnify-around-target-overlay
                        'priority 100))
          ((and $delete swoop-magnify-around-target-overlay)
           (delete-overlay swoop-magnify-around-target-overlay))
          (t
           (move-overlay
            swoop-magnify-around-target-overlay
            (line-beginning-position (- 0 $around))
            (line-beginning-position $around))))))
(defsubst swoop--move-line-within-target-window ($line-num)
  (with-selected-window swoop-target-window
    (swoop--goto-line $line-num)
    (move-overlay
     swoop-target-buffer-selection-overlay
     (point) (min (1+ (line-end-position)) (point-max)))
    (if swoop-use-target-magnifier:
        (swoop--magnify-around-target))
    (swoop--unveil-invisible-overlay)
    (recenter)))
(defsubst swoop--boblp (&optional $point)
  (save-excursion
    (goto-char (point-min))
    (eq (line-number-at-pos)
        (progn (goto-char (or $point (point))) (line-number-at-pos)))))
(defsubst swoop--eoblp (&optional $point)
  (save-excursion
    (goto-char (point-max))
    (eq (line-number-at-pos)
        (progn (goto-char (or $point (point))) (line-number-at-pos)))))
(defsubst swoop--forward-visible-line ()
  (unless (and (bobp) (eobp))
    (if (not (eobp))
        (let ((buffer-invisibility-spec '(t))
              ($pos (point)))
          (save-excursion
            (while (progn
                     (or (setq $pos (next-single-property-change
                                     $pos 'invisible))
                     (setq $pos (re-search-forward "\n.+$" nil t)))
                     (and (eq 'swoop (get-text-property $pos 'invisible))
                          (not (swoop--eoblp $pos))))))
          (when (not (eq 'swoop (get-text-property $pos 'invisible)))
            (goto-char $pos)
            (goto-char (line-beginning-position)))))))
(defsubst swoop--backward-visible-line ()
  (unless (and (bobp) (eobp))
    (if (not (bobp))
        (let ((buffer-invisibility-spec '(t))
              ($pos (point)))
          (save-excursion
            (while (progn
                     (or (setq $pos (previous-single-property-change
                                    $pos 'invisible))
                     (setq $pos (re-search-backward "\n" nil t)))
                     (and (eq 'swoop (get-text-property $pos 'invisible))
                          (not (swoop--boblp $pos))))))
          (when (not (eq 'swoop (get-text-property $pos 'invisible)))
            (goto-char $pos)
            (goto-char (line-beginning-position)))))))

(cl-defsubst swoop--move-line (&optional $init)
  (with-selected-window swoop-window
    (let ($line-num)
      (cl-case $init
        (up   (swoop--backward-visible-line))
        (down (swoop--forward-visible-line))
        (init (cond
               ((and (bobp) (eobp))
                (cl-return))
               ((bobp)
                (swoop--forward-visible-line)
                (move-beginning-of-line 1))
               ((eobp)
                (swoop--backward-visible-line)
                (move-beginning-of-line 1))
               (t (move-beginning-of-line 1))
               )))
      (move-overlay
       swoop-buffer-selection-overlay
       (point) (min (1+ (line-end-position)) (point-max)))
      (unless (or (bobp) (eobp))
        (swoop--move-line-within-target-window (line-number-at-pos)))
      (recenter))))

(defsubst swoop--next-line ()
  (interactive)
  (swoop--move-line 'down))
(defsubst swoop--prev-line ()
  (interactive)
  (swoop--move-line 'up))
(defsubst swoop--invisible-on ()
  (add-to-invisibility-spec 'swoop))
(defsubst swoop--invisible-off ()
  (remove-from-invisibility-spec 'swoop))


(defsubst swoop--buffer-selection-overlay-set ()
  (setq swoop-buffer-selection-overlay
        (make-overlay (line-beginning-position)
                      (min (1+ (line-end-position)) (point-max))))
  (overlay-put swoop-buffer-selection-overlay 'face 'swoop-target-line-face)
  (overlay-put swoop-buffer-selection-overlay 'priority 15))
(defsubst swoop--target-buffer-selection-overlay-set ()
  (setq swoop-target-buffer-selection-overlay
        (make-overlay (line-beginning-position)
                      (min (1+ (line-end-position)) (point-max))))
  (overlay-put swoop-target-buffer-selection-overlay
               'face 'swoop-target-line-face)
  (overlay-put swoop-target-buffer-selection-overlay 'priority 15))

(defvar swoop-point-at-function (lambda () (thing-at-point 'symbol)))
(defvar swoop-target-buffer-overlay nil)
(defvar swoop-buffer-content "")
(cl-defun swoop--core (&key $query $resume)
  (setq swoop--last-position (point))
  (setq swoop-target-buffer (current-buffer))
  (setq swoop-target-window (get-buffer-window swoop-target-buffer))
  (setq swoop-buffer-content
        (buffer-substring-no-properties (point-min) (point-max)))
  ;; Font size change
  (setq swoop-target-buffer-overlay (make-overlay (point-min) (point-max)))
  (overlay-put swoop-target-buffer-overlay
               'face `(:height ,swoop-font-size-change:))
  (recenter)
  (swoop--target-buffer-selection-overlay-set)
  (save-window-excursion
    (let* (($po (point))
           ($pos-min (point-min)))
      (funcall swoop-display-function swoop-buffer)
      ;; (erase-buffer)
      ;; (font-lock-mode 1) ;; for hi-lock
      (insert (swoop--modify-buffer-content swoop-buffer-content))
      (setq swoop-window (get-buffer-window swoop-buffer))
      (goto-char $po)
      (swoop--buffer-selection-overlay-set))
    (let ((buffer-invisibility-spec '(t))) ;; temporary unvail text for org-mode, or etc
      (unwind-protect
          (when (get-buffer swoop-buffer)
            (unless $resume
              (ht-clear! swoop--async-box))
            (when (or $query $resume)
              ;; Prevent following minibuffer session
              (setq swoop--minibuf-last-content $query)
              (setq swoop--minibuffer-default-input $query)
              ;; First time
              (if (or (listp $query) $resume)
                  (swoop-update swoop--last-query-converted swoop-buffer)
                (swoop-update (split-string $query " " t) swoop-buffer)))
            (swoop--read-from-string $query swoop-buffer)
            ;; (setq swoop--minibuffer-default-input nil)
            )

        ;; (swoop--clear-overlay)
        (when (get-buffer swoop-buffer)
          (swoop--clear-overlay :$kill t)
          (swoop--invisible-off))
        (delete-overlay swoop-target-buffer-overlay)))))

(defun swoop--pre-input (&optional $resume)
  (let ($results)
    (if $resume
        (setq $results swoop--last-query-plain)
      (setq $results (cond (mark-active
                            (buffer-substring-no-properties
                             (region-beginning) (region-end)))
                           ((funcall swoop-point-at-function))
                           (t nil)))
      (deactivate-mark))
    $results))
;;;###autoload
(defun swoop (&optional $query)
  (interactive)
  (if current-prefix-arg
      (swoop--core :$resume t)
    (swoop--core :$query (or $query (swoop--pre-input)))))
;;;###autoload
(defun swoop-pcre-regexp (&optional $query)
  (interactive)
  (let ((swoop-use-pcre t))
    (if current-prefix-arg
        (swoop--core :$resume t)
      (swoop--core :$query (or $query (swoop--pre-input))))))
;;;###autoload
(defun swoop-migemo (&optional $query)
  (interactive)
  (let ((swoop-use-migemo t))
    (if current-prefix-arg
        (swoop--core :$resume t)
      (swoop--core :$query (or $query (swoop--pre-input))))))

(defun my-delete/clear-all-highlight-lock ()
  (interactive)
  (swoop--mapc $hi hi-lock-interactive-patterns
    (when hi-lock-interactive-patterns
      (font-lock-remove-keywords nil (list $hi))
      (setq hi-lock-interactive-patterns nil))
    ;; (when font-lock-fontified (font-lock-fontify-buffer)))
  (when font-lock-fontified (font-lock-fontify-buffer))))
(cl-defun swoop--clear-overlay (&key $to-empty $kill)
  (swoop--mapc $buf (list swoop-target-buffer swoop-buffer)
    (when (get-buffer swoop-buffer)
      (with-current-buffer $buf
        (my-delete/clear-all-highlight-lock)
        (swoop--mapc $ov (overlays-in (point-min) (point-max))
          (when (overlay-get $ov 'swoop-temporary)
            (delete-overlay $ov))))))
  (if swoop-use-target-magnifier:
      (swoop--magnify-around-target :$delete t))
  (unless $to-empty
    (delete-overlay swoop-target-buffer-selection-overlay)
    (unless $kill
      (delete-overlay swoop-buffer-selection-overlay)))
  (if (and $kill (get-buffer swoop-buffer))
      (kill-buffer swoop-buffer)))

;; (swoop-update "" (current-buffer))
;; (swoop-update "d" (current-buffer))

(defvar swoop--keep-buffer-position 1)
(defun swoop-update ($query $buf)
  (when (get-buffer $buf)
    (unless (listp $query)
      (setq $query (split-string $query " " t)))
    (setq swoop--last-query-converted $query)
    (unwind-protect
        (with-current-buffer $buf
          (let ((inhibit-modification-hooks t))
            (if (not $query)
                (progn
                  (put-text-property (point-min) (point-max) 'invisible nil)
                  (swoop--clear-overlay :$to-empty t)
                  (swoop--invisible-off))
              (setq swoop--keep-buffer-position (point))
              (swoop--clear-overlay)
              ;; (swoop--invisible-off)
              (put-text-property (point-min) (point-max) 'invisible 'swoop)
              (swoop-words-overlay $query)
              (swoop--invisible-on)
              ;; Turn on invisible
              (swoop--move-line 'init)))))))


(defvar swoop--last-visible-lines nil)
(defun swoop--match-lines-list-common ($match-lines-list)
  "Return common numbers list of several numbers lists.
 (swoop--match-lines-list-common '((1 2 3 4 5) (2 3 4 8) (2 3 9))) -> '(2 3)"
  (let* (($list $match-lines-list)
         ($results)
         ($length (length $list)))
    (when (> $length 0)
      (setq $results (car-safe $list))
      (if (> $length 1)
          (swoop--mapc $l (cdr $list)
            (setq $results (-intersection $results $l)))))
    (setq swoop--last-visible-lines $results)))


;; (swoop--get-match-lines-list-async "a")
(cl-defun swoop--get-match-lines-list-async ($query &optional $from)
  (save-excursion
    (goto-char (point-min))
    (let* (($po (re-search-forward $query nil t))
           ($match-lines nil)
           ($return-match (bolp)))
      (cl-block stop
        (while $po
          (setq $match-lines
                (cons
                 (if $return-match
                     ;; include end of return match (e.g. def[^u])
                     (1- (+ (line-number-at-pos) $from))
                   (+ (line-number-at-pos) $from))
                 $match-lines))
          (forward-line 1)
          (if (eq $po (setq $po (re-search-forward $query nil t)))
              (cl-return-from stop nil))))
      $match-lines)))
(setq swoop--async-fn (byte-compile 'swoop--get-match-lines-list-async))

(cl-defun swoop--words-overlay ($pattern $line-format)
  ;; Delete counting key
  (ht-remove! swoop--async-box swoop--async-latest-tag)
  (setq ff (ht-amap value swoop--async-box))
  ;; (ht-items swoop--async-box)
  (swoop--mapc $l (swoop--match-lines-list-common
                   ;; Make match only list
                   (ht-amap value swoop--async-box))
    (swoop--goto-line $l)
    ;; (overlay-put (make-overlay (line-beginning-position) (1+ (line-end-position)))
    ;;              'invisible 'swoop)
    (let* (($lbeg (line-beginning-position))
           ($lend (line-end-position))
           ($lov (make-overlay $lbeg $lend)))
      ;; Show lines
      (put-text-property $lbeg (min (1+ $lend) (point-max)) 'invisible nil)

      ;; Line number overlay
      (overlay-put $lov 'before-string
                   (propertize
                    (format $line-format $l)
                    'face '(:foreground "#ff9900")))
      (overlay-put $lov 'swoop-temporary t)

      (if nil
          (progn
            (hi-lock-set-pattern $pattern 'swoop-target-words-face)
            (with-selected-window swoop-target-window
              (hi-lock-set-pattern $pattern 'swoop-target-words-face)))

            (cl-block stop
              (while (re-search-forward $pattern $lend t)
                (let* (($wbeg (match-beginning 0))
                       ($wend (match-end 0))
                       ($ov (make-overlay $wbeg $wend)))
                  (overlay-put $ov 'face 'swoop-target-words-face)
                  (overlay-put $ov 'swoop-temporary t)
                 (overlay-put $ov 'priority 20)
                  (with-selected-window swoop-target-window
                    (setq $ov (make-overlay $wbeg $wend))
                    (overlay-put $ov 'face 'swoop-target-words-face)
                    (overlay-put $ov 'swoop-temporary t)
                    (overlay-put $ov 'priority 20))
                  ))))

      ))
  (with-selected-window swoop-window
    ;; Keep position near where the cursor was at before update the list
    (goto-char swoop--keep-buffer-position)
    (swoop--move-line 'init)
    ;; Back to inner list if the cursor at the top or at the bottom of the list
    (cond ((and (bobp) (eobp))
           nil)
          ((bobp)
           (swoop--next-line))
          ((eobp)
           (swoop--prev-line)))
    ))

(defun swoop--get-point-from-line ($line &optional $buf)
  (or $buf (setq $buf (current-buffer)))
  (save-excursion
    (with-current-buffer $buf
      (swoop--goto-line $line)
      ;; Must subtract 1 for extract buffer contents, by substring-no-properties
      (1- (point)))))

(defun swoop--async-count ($result $length $pattern $line-format)
  ;; (setq a (list (caar $result) (cdar $result) (cdr $result)))
  ;; (setq b (list $length $pattern $line-format))
  (let* (($tag (car $result))
         ($check-key (car $tag)))
    (if (equal swoop--async-latest-tag $check-key)
        (let (($key (cdr $tag))
              ($match (cdr $result)))
          (progn
            (if (ht-contains? swoop--async-box $check-key)
                (ht-set swoop--async-box
                        $check-key
                        (1+ (ht-get swoop--async-box $check-key)))
              (ht-set swoop--async-box $check-key 1))
            (if (ht-contains? swoop--async-box $key)
                ;; Add results if the same $key already exists
                (ht-set swoop--async-box
                        $key
                        (append
                         (ht-get swoop--async-box $key)
                         $match))
              (ht-set swoop--async-box $key $match))
            (setq p (ht-items swoop--async-box))
            (if (eq $length (ht-get swoop--async-box $check-key))
                (swoop--words-overlay $pattern $line-format)))))))
;; (swoop--async-count (cons swoop--async-latest-tag '(9 8 7 6)) 3)

(setq swoop--async-box (make-hash-table :test 'equal))
(defvar swoop--async-latest-tag nil)

(cl-defun swoop-words-overlay ($query)
  (let* (($buf (current-buffer)) ;; Must be swoop-buffer
         ($bufcont swoop-buffer-content)
         ($mhhatch-lines-list nil)
         ($max (point-max))
         ($length (length $query))
         ($max-line (line-number-at-pos $max))
         ($max-line-digit
          (length (number-to-string $max-line)))
         ($line-format (concat "%0" (number-to-string
                                     $max-line-digit) "s: "))
         ($pattern (concat
                    "\\(" (mapconcat 'identity $query "\\|")
                    "\\)"))
         ($lb 300)
         ($ln (/ $max-line $lb))
         ($lr (% $max-line $lb))
         ($bn (if (eq 0 $lr)
                  $ln
                (1+ $ln)))
         ($plength (* $bn $length))
         ($tag2))
    (setq swoop--async-latest-tag (symbol-name (cl-gensym)))
    (setq aaa (list $lb $ln $lr $bn $plength 99))
    (setq dd nil)
    ;; (setq aaa (list (1+ (* 0 $bn)) (1+ (* (1+ 0) $lb)) $lr $bn $plength))
    (if (ht-empty? swoop--async-box)
        (save-excursion
          (swoop--mapc $q $query
            (setq $tag2 (symbol-name (cl-gensym)))
            (cl-dotimes ($i $bn)
              (setq dd
                    (cons (list (1+ (* $i $lb)) (min $max-line (* (1+ $i) $lb)))
                          dd))
              ;; (setq aaa (list (1+ (* $i $lb)) (* (1+ $i) $lb) $lr $bn $plength 99))
              (async-start
               `(lambda ()
                  (fundamental-mode)
                  (insert ,(substring-no-properties
                            $bufcont
                            (swoop--get-point-from-line (1+ (* $i $lb)))
                            (swoop--get-point-from-line
                             (min $max-line (* (1+ $i) $lb)))))
                  (goto-char (point-min))
                  (cons (cons ,swoop--async-latest-tag ,$tag2)
                        (funcall ,swoop--async-fn ,$q ,(* $i $lb))))
               `(lambda ($result)
                  (when (get-buffer ,$buf)
                    (with-current-buffer ,$buf
                      (swoop--async-count $result ,$plength ,$pattern ,$line-format)
                      ))
                  )))))
      ;; Resume hash
      (when (get-buffer $buf)
        (with-current-buffer $buf
          (swoop--words-overlay $pattern $line-format))))))


;; \w{2,3}.html?$
;; (swoop--pcre-convert (read-string "PCRE: " "\\w{2,3}.html?$"))
;; ^\s*\w \d{2,3}
;; (swoop--pcre-convert (read-string "PCRE: " "^\\s*\\w \\d{2,3}"))
(defvar swoop-use-pcre nil)
(defsubst swoop--pcre-convert ($query)
  (nreverse
   (swoop--mapcr $q (split-string $query " " t)
     (rxt-pcre-to-elisp $q))))

;; (swoop--migemo-convert "kaki kuke")
;; (swoop--migemo-convert "kakuku")
(defvar swoop-use-migemo nil)
(defvar swoop-migemo-options "-q -e -d /usr/local/share/migemo/utf-8/migemo-dict")
(defsubst swoop--migemo-convert ($query)
  (if (executable-find "cmigemo")
      (nreverse
       (swoop--mapcr $q (split-string $query " " t)
         (replace-regexp-in-string
          "\n" ""
          (shell-command-to-string
           (concat "cmigemo" " -w " $q " " swoop-migemo-options))))))
  (error "cmigemo not found..."))
(defun swoop--convert-input ($input)
  (cond
   ;; PCRE
   ((and swoop-use-pcre
         (not swoop-use-migemo))
    (setq $input (swoop--pcre-convert $input)))
   ;; MIGEMO
   ((and swoop-use-migemo
         (not swoop-use-pcre))
    (setq $input (swoop--migemo-convert $input))))
  $input)

(setq swoop--minibuffer-default-input nil)
(setq swoop-input-dilay 0.1)
(setq swoop-input-threshold 2)
(defun swoop--read-from-string ($query $buf)
  (let (($timer nil))
    (unwind-protect
        (minibuffer-with-setup-hook
            (lambda ()
              (setq
               $timer
               (run-with-idle-timer
                swoop-input-dilay
                'repeat
                (lambda ()
                  (with-selected-window (or (active-minibuffer-window)
                                            (minibuffer-window))
                    (let* (($content (minibuffer-contents)))
                      (when (and (not (equal swoop--minibuf-last-content
                                             $content))
                                 (or
                                  ;; When becomeing empty again
                                  (equal "" $content)
                                  ;; Avoid too many matching
                                  (>= (length $content) swoop-input-threshold)))
                        ;; Stop old async process
                        (ht-clear! swoop--async-box)
                        (setq swoop--minibuf-last-content $content)
                        (swoop-update (swoop--convert-input $content)
                                      $buf))))))))
          (read-from-minibuffer
           "Swoop: " (or $query "") swoop-map nil
           query-replace-from-history-variable
           swoop--minibuffer-default-input t))
      (when $timer (cancel-timer $timer) (setq $timer nil))
      (setq swoop--last-query-plain swoop--minibuf-last-content)
      (setq swoop--minibuf-last-content "")
      ;; (with-current-buffer swoop-buffer
      ;;   (put-text-property (point-min) (point-max) 'invisible nil)
      ;;   (remove-text-properties (point-min) (point-max) 'swoop))
      (recenter))))

;; MIGEMO test
;; 福山慎吾
;; あいうえお　かきくけこ
;; あいうえお　さしすせそ
;; ああああああああ

;; @ Edit mode -------------------------------------------------------------

(defvar swoop-edit-map
  (let (($map (make-sparse-keymap)))
    ;; (define-key $map (kbd "C-x C-s") 'swoop--edit-apply-changes)
    (define-key $map (kbd "C-x C-s") 'swoop--edit-finish)
    (define-key $map (kbd "C-c C-c") 'swoop--edit-finish)
    $map))
(define-key swoop-map (kbd "C-c C-e") 'swoop--edit)

(defvar swoop-edit-buffer "*Swoop Edit*")

(defun swoop--edit-finish ()
  (interactive)
  (select-window swoop-target-window)
  (kill-buffer swoop-edit-buffer))

(defun swoop--modify-buffer-content ($bufcont)
  "Modify the original buffer content, but it causes slow rendering."
  $bufcont)

;; (swoop--line-beg-point 400)
(defsubst swoop--line-beg-point ($line &optional $buf)
  (with-current-buffer (or $buf (current-buffer))
    (save-excursion
      (swoop--goto-line $line) (point))))
(defsubst swoop--set-marker ($line &optional $buf)
  (with-current-buffer (or $buf (current-buffer))
    (save-excursion
      (swoop--goto-line $line)
      (set-marker (make-marker) (point)))))


(defun swoop--edit-set-properties ($buf)
  "Set edit buffer format"
  (save-excursion
    (goto-char (point-min))
    (add-text-properties (point-min) (point-max)
                         '(read-only t rear-nonsticky t front-sticky t))
    (let* ((inhibit-read-only t)
           ($po (point))
           ($max-line-digit
            (length (number-to-string (line-number-at-pos (buffer-end 1)))))
           $po $eol)
      (while (setq $po (re-search-forward
                         "^[[:space:]]*[1-9][0-9]*::[[:space:]]" nil t))
        (end-of-line)
        (setq $eol (point))
        ;; (put-text-property (line-beginning-position) $eol
        ;;                    'swoop-target
        ;;                    (swoop--set-marker (line-number-at-pos) $buf))
        ;; Make editable area
        (remove-text-properties $po $eol '(read-only t))
        ;; For line trailing return
        (set-text-properties $eol (1+ $eol)
                             '(read-only t rear-nonsticky t))))))

(defun swoop--get-match-line-content ($buf $visible-lines)
  " `$visible-lines' is like '(20 30 33 50 ...)"
  (with-current-buffer $buf
    (let ($results
          ($max-line-digit
           (length (number-to-string (line-number-at-pos (point-max))))))
      (save-excursion
        (swoop--mapc $l $visible-lines
                (swoop--goto-line $l)
                (setq $results
                      (cons
                       (cons
                        (propertize
                         (format (concat
                                  "%0"
                                  (number-to-string $max-line-digit)
                                  "s:: ") $l)
                         'face 'swoop-line-number-face
                         'swoop-prefix t
                         'swoop-target (set-marker (make-marker) (point))
                         'intangible t
                         'rear-nonsticky t)
                        (buffer-substring
                         (line-beginning-position) (line-end-position)))
                       $results))))
      $results)))
;;(swoop--get-match-line-content (current-buffer) swoop--last-visible-lines)
(defun swoop--edit-insert-lines ($buf $visible-lines)
  (dolist ($l (swoop--get-match-line-content $buf $visible-lines))
    (insert (format "%s%s\n" (car $l) (cdr $l)))))
;; (swoop--edit-insert-lines swoop--last-visible-lines)

(defun swoop--edit ()
  (interactive)
  (let (($bufcont (with-current-buffer swoop-buffer
                    (buffer-substring
                     (point-min) (point-max)))))
    (run-with-timer
     0 nil
     (lambda ($bufcont $bufname $bufwindow $visible-lines)
       (when (get-buffer swoop-edit-buffer)
         (kill-buffer swoop-edit-buffer))
       (funcall swoop-display-function swoop-edit-buffer)
       (erase-buffer)
       (set (make-local-variable 'swoop-target-buffer) $bufname)
       (set (make-local-variable 'swoop-target-window) $bufwindow)
       ;; Header
       (insert (propertize
                (concat " " $bufname "\n")
                'face '(:height 1.5 :background "#333333" :foreground "#eeeeee")
                'intangible t))
       ;; Body
       (swoop--edit-insert-lines $bufname $visible-lines)
       ;; Set properties
       (swoop--edit-set-properties $bufname)
       ;; Goto first editable point
       (goto-char (point-min))
       (forward-line 1)
       ;; (re-search-forward "^[[:space:]]*\\([0-9]+\\)::[[:space:]]" nil t)
       (add-hook 'after-change-functions
                 'swoop-after-change-function nil t)
       (use-local-map swoop-edit-map))
     ;; Args
     $bufcont
     (buffer-name swoop-target-buffer)
     swoop-target-window
     swoop--last-visible-lines)
    (exit-minibuffer)))

(defun swoop-after-change-function ($beg $end $length)
  (save-excursion
    (goto-char $beg)
    (let* (($line-beg (line-beginning-position))
           ($m (get-text-property $line-beg 'swoop-target))
           ($buf (marker-buffer $m))
           $col)
      (when (and (get-text-property $line-beg 'swoop-prefix)
                 (not (get-text-property $end 'swoop-prefix)))
        (when (= $length 0)
          ;; Apply swoop-target property to inserted (e.g. yanked) text.
          (put-text-property $beg $end 'swoop-target $m)
          ;; Did we insert a newline?  Swoop Edit mode can't create new
          ;; Swoop entries; just discard everything after the newline.
          (save-excursion
            (and (re-search-forward "\n" $end t)
                 (delete-region (1- (point)) $end))))
        (let* (($line (- (line-number-at-pos)
                        (line-number-at-pos (window-start))))
               ($readonly (with-current-buffer $buf buffer-read-only))
               ($win (or (get-buffer-window $buf)
                        (display-buffer $buf
                                        '(nil (inhibit-same-window . t)
                                              (inhibit-switch-frame . t)))))
               ($line-end (line-end-position))
               ($text (save-excursion
                       (goto-char (next-single-property-change
                                   $line-beg 'swoop-prefix nil
                                   $line-end))
                       (setq $col (- (point) $line-beg))
                       (buffer-substring-no-properties (point) $line-end))))
          (with-selected-window $win
            (goto-char $m)
            ;; Unveil invisible block
            (swoop--mapc $ov
                (overlays-in (line-beginning-position)
                             (line-end-position))
              (let (($type (overlay-get $ov 'invisible)))
                (when $type
                  (overlay-put $ov 'invisible nil))))
            (recenter $line)
            (if $readonly
                (message "Buffer `%s' is read only." $buf)
              (delete-region (line-beginning-position) (line-end-position))
              (insert $text))
            (move-to-column $col)))))))

(provide 'swoop)
;;; swoop.el ends here


(setq fi 99)
(async-start
 `(lambda ()
    ;; (insert ,(buffer-substring-no-properties (point-min) (point-max)))
    ;; (goto-char (point-min))
    ;; (funcall ,swoop--async-fn "a")
    (insert "aaa")
    (setq ttt (make-hash-table :test 'equal))
    )
 `(lambda (result)
    (princ (cons (type-of result) result))
    ))

;;aaaa
;; (setq ppp (buffer-substring-no-properties (line-beginning-position 1) (line-end-position)))



;; (echo ppp)
;; (echo (buffer-substring-no-properties (swoop--get-point-from-line 864) (swoop--get-point-from-line 865)))












;; end
