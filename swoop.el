
(require 'dash)
(require 'hi-lock)
;; (my-nest-specific-text-property-position 'invisible 'swoop)
;; buffer-invisibility-spec

(defgroup swoop nil
  "Group for swoop"
  :prefix "swoop-" :group 'convenience)

(defvar swoop-buffer "*Swoop*")
(defvar swoop-window nil)
(defvar swoop-target-buffer nil)
(defvar swoop-target-window nil)
(defvar swoop-buffer-word-overlay nil)
(defvar swoop-target-buffer-word-overlay nil)

(defvar swoop--last-position 1)
(defvar swoop--cached-count 0)
(defvar swoop--minibuf-last-content "")
(defvar swoop--last-query "")
(setq swoop-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "C-n") 'swoop--next-line)
    (define-key map (kbd "C-p") 'swoop--prev-line)
    (define-key map (kbd "C-c C-e") 'swoop--edit)
    (define-key map (kbd "C-g") 'swoop--cancel)
    (define-key map (kbd "RET") 'swoop--default-action)
    (define-key map (kbd "<C-return>") 'swoop--default-action)
    map))

(defun swoop--cancel ()
  "This is assigned to `C-g' as default. Exit from Swoop (minibuffer),
and execute functions listed in swoop-abort-hook"
  (interactive)
  (run-with-timer
   0 nil (lambda () (run-hooks 'swoop-abort-hook)))
  (exit-minibuffer))

(defvar swoop-abort-hook nil)
(defun swoop-back-to-last-position ()
  (interactive)
  (let (($pos swoop--last-position))
    (setq swoop--last-position (point))
    (with-selected-window swoop-target-window
      (goto-char $pos))))
(add-hook 'swoop-abort-hook 'swoop-back-to-last-position)

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
  (declare (indent 2))
  (let (($list-unique (gensym)))
    `(let ((,$list-unique ,$list))
       (mapc (lambda (,$variable)
               ,@$body)
             ,$list-unique))))
(defmacro swoop--mapcr ($variable $list &rest $body)
  (declare (indent 2))
  (let (($list-unique (gensym)))
    `(let ((,$list-unique ,$list)
           ($results))
       (mapc (lambda (,$variable)
               (setq $results (cons (progn ,@$body) $results)))
             ,$list-unique)
       $results)))
;; (let ($c)
;;   (swoop--mapc $a '(1 2 3)
;;     (setq $c (cons (+ $a $a) $c)))
;;   $c) ;; (6 4 2)
;; (swoop--mapcr $a '(1 2 3) (* $a $a)) ;; (6 4 2)

(defun swoop--default-action () (interactive)
  (run-with-timer
   0 nil
   (lambda ($pos)
     (with-selected-window swoop-target-window
       (goto-char $pos)
       (save-excursion (re-search-forward
                        (concat "\\("
                                (mapconcat 'identity swoop--last-query "\\|")
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

(setq swoop-window-split-current-window nil)
;; 'split-window-horizontally 'split-window-vertically
(setq swoop-window-split-direction 'split-window-vertically)
(setq swoop-display-function
  (lambda ($buf)
    (if swoop-window-split-current-window
        (funcall swoop-window-split-direction)
      (when (one-window-p)
        (funcall swoop-window-split-direction)))
    (other-window 1)
    (switch-to-buffer $buf)))
;; (funcall swoop-display-function "aaa")


(defvar swoop-invisible-targets nil)
(defsubst swoop--restore-unveiled-overlay ()
  (when swoop-invisible-targets
    (swoop--mapc $ov (cdr $ov) (overlay-put (car $ov) 'invisible (cdr $ov)))
          swoop-invisible-targets)
    (setq swoop-invisible-targets nil))
(defsubst swoop--unveil-invisible-overlay ()
  "Show hidden text temporarily to view it during swoop.
This function needs to call after latest swoop-target-buffer-word-overlay moved."
  (swoop--restore-unveiled-overlay)
  (swoop--mapc $ov
      (overlays-in (overlay-start swoop-target-buffer-word-overlay)
                   (overlay-end swoop-target-buffer-word-overlay))
    (let (($type (overlay-get $ov 'invisible)))
      (when $type
        (overlay-put $ov 'invisible nil)
        (setq swoop-invisible-targets
              (cons (cons $ov $type) swoop-invisible-targets))))))
(defsubst swoop--goto-line ($line)
  (goto-char (point-min))
  (unless (re-search-forward "\n" nil t (1- $line))
    (goto-char (point-max))))
(defsubst swoop--move-line-within-target-window ($line-num)
  (with-selected-window swoop-target-window
    (swoop--goto-line $line-num)
    (move-overlay
     swoop-target-buffer-word-overlay
     (point) (min (1+ (line-end-position)) (point-max)))
    (swoop--unveil-invisible-overlay)
    (recenter)))
(defsubst swoop--forward-visible-line ()
  (unless (and (bobp) (eobp))
    (if (not (eobp))
        (let ((buffer-invisibility-spec '(t))
              $pos)
          (save-excursion
            (while (progn
                     (setq $pos (re-search-forward "\n.+$" nil t))
                     (and (eq 'swoop (get-text-property $pos 'invisible))
                          (not (eobp))))))
          (when (not (eq 'swoop (get-text-property $pos 'invisible)))
            (goto-char $pos)
            (goto-char (line-beginning-position)))))))
(defsubst swoop--backward-visible-line ()
  (unless (and (bobp) (eobp))
    (if (not (bobp))
        (let ((buffer-invisibility-spec '(t))
              $pos
              ($pos-min (point-min)))
          (save-excursion
            (while (progn (setq $pos (re-search-backward "\n" nil t))
                          (and (eq 'swoop (get-text-property $pos 'invisible))
                               (not (bobp))))))
          (when (not (eq 'swoop (get-text-property $pos 'invisible)))
            (goto-char $pos)
            (goto-char (line-beginning-position)))))))
(defsubst swoop--move-line (&optional $init)
  (with-selected-window swoop-window
    (let ($line-num)
      (cl-case $init
        (up   (swoop--backward-visible-line))
        (down (swoop--forward-visible-line))
        (init (cond ((bobp)
                     (swoop--forward-visible-line)
                     (move-beginning-of-line 1))
                    ((eobp)
                     (swoop--backward-visible-line)
                     (move-beginning-of-line 1))
                    (t (move-beginning-of-line 1)))))
      (move-overlay
       swoop-buffer-word-overlay
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
  (setq swoop-buffer-word-overlay
        (make-overlay (line-beginning-position)
                      (min (1+ (line-end-position)) (point-max))))
  (overlay-put swoop-buffer-word-overlay 'face 'swoop-target-line-face)
  (overlay-put swoop-buffer-word-overlay 'priority 15))
(defsubst swoop--target-buffer-selection-overlay-set ()
  (setq swoop-target-buffer-word-overlay
        (make-overlay (line-beginning-position)
                      (min (1+ (line-end-position)) (point-max))))
  (overlay-put swoop-target-buffer-word-overlay
               'face 'swoop-target-line-face)
  (overlay-put swoop-target-buffer-word-overlay 'priority 15))

(global-set-key (kbd "C-o") 'swoop)
(global-set-key (kbd "C-S-o") 'swoop-back-to-last-position)
(global-set-key (kbd "M-o") 'swoop-pcre-regexp)
(defvar swoop-point-at-function (lambda () (thing-at-point 'symbol)))
(defvar swoop-target-buffer-overlay nil)
(cl-defun swoop--core (&key $query)
  (setq swoop--last-position (point))
  (setq swoop-target-buffer (current-buffer))
  (setq swoop-target-window (get-buffer-window swoop-target-buffer))
  (setq swoop-target-buffer-overlay (make-overlay (point-min) (point-max)))
  (overlay-put swoop-target-buffer-overlay 'face '(:height 0.9))
  (swoop--target-buffer-selection-overlay-set)
  (save-window-excursion
    (let* (($pos (point))
           ($pos-min (point-min))
           ($bufcont (buffer-substring-no-properties $pos-min (point-max))
                     ;;(buffer-substring $pos-min (point-max))
                     ))
      (funcall swoop-display-function swoop-buffer)
      (erase-buffer)
      (font-lock-mode 1) ;; for hi-lock
      (setq $bufcont (swoop--modify-buffer-content $bufcont))
      (insert $bufcont)
      ;; (linum-mode 1)
      ;;(swoop-line-number-overlay)
      (setq swoop-window (get-buffer-window swoop-buffer))
      (goto-char $pos)
      (swoop--buffer-selection-overlay-set))
    (let ((buffer-invisibility-spec '(t))) ;; temporary unvail text for org-mode, or etc
      (unwind-protect
          (progn
            (when $query
              (setq swoop--minibuf-last-content $query)
              (if (listp $query)
                  (swoop-update swoop--last-query swoop-buffer)
                (swoop-update (split-string $query " " t) swoop-buffer)))
            (swoop--read-from-string $query swoop-buffer))
        (swoop--clear-overlay :$kill t)
        (swoop--invisible-off)))))
(defun swoop--pre-input ()
  (let ($results)
    (setq $results (cond (mark-active
                          (buffer-substring-no-properties
                           (region-beginning) (region-end)))
                         ((funcall swoop-point-at-function))
                         (t nil)))
    (deactivate-mark)
    $results))
(defun swoop (&optional $query)
  (interactive)
  ;; (if current-prefix-arg
    (swoop--core :$query (or $query (swoop--pre-input))))
(defun swoop-pcre-regexp (&optional $query)
  (interactive)
  (let ((swoop-use-pcre t))
    ;; (if current-prefix-arg
    (swoop--core :$query (or $query (swoop--pre-input)))))
(defun swoop-migemo (&optional $query)
  (interactive)
  (let ((swoop-use-migemo t))
    ;; (if current-prefix-arg
    (swoop--core :$query (or $query (swoop--pre-input)))))

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
    (with-current-buffer $buf
      (my-delete/clear-all-highlight-lock)
      (swoop--mapc $ov (overlays-in (point-min) (point-max))
                 (when (overlay-get $ov 'swoop-temporary)
                   (delete-overlay $ov))))
  (delete-overlay swoop-target-buffer-overlay)
  (unless $to-empty
    (delete-overlay swoop-target-buffer-word-overlay)
    (unless $kill
      (delete-overlay swoop-buffer-word-overlay)))
  (if $kill
      (kill-buffer swoop-buffer))))

;; (swoop-update "" (current-buffer))
;; (swoop-update "d" (current-buffer))

(defvar swoop-halt-search-process nil)
(defun swoop-update ($query $buf)
  (unless (listp $query)
    (setq $query (split-string $query " " t)))
  (setq swoop--last-query $query)
  (setq swoop-halt-search-process t)
  (unwind-protect
      (with-current-buffer $buf
        (let ((inhibit-modification-hooks t))
          (if (not $query)
              (progn
                (put-text-property (point-min) (point-max) 'invisible nil)
                (swoop--clear-overlay :$to-empty t)
                (swoop--invisible-off))
            (setq swoop-halt-search-process nil)
            (swoop--clear-overlay)
            (swoop--invisible-off)
            (put-text-property (point-min) (point-max) 'invisible 'swoop)
            (swoop-words-overlay $query)
            ;; Turn on invisible
            (swoop--invisible-on)
            (swoop--move-line 'init)

            ;; Adjust swoop buffer cursor position
            (with-selected-window swoop-window
              (unless (and (bobp) (eobp))
                (if (bobp)
                    (swoop--move-line 'down))
                (if (eobp)
                    (swoop--move-line 'up))))
            )))))


(setq swoop--last-visible-lines nil)
;; (swoop--match-lines-list-common '((1 2 3 4 5) (2 3 4 8) (2 3 9)))
(defun swoop--match-lines-list-common ($match-lines-list)
  "Return common numbers list of several numbers lists.
'((1 2 3 4 5) (2 3 4 8) (2 3 9)) -> '(2 3)"
  (let* (($list $match-lines-list)
         ($results)
         ($length (length $list)))
    (when (> $length 0)
      (setq $results (car-safe $list))
      (if (> $length 1)
          (swoop--mapc $l (cdr $list)
            (setq $results (-intersection $results $l)))))
    (setq swoop--last-visible-lines $results)))


(cl-defun swoop-words-overlay ($query)
  (let* (($match-lines-list nil)
         ($max (point-max))
         ($max-line-digit
          (length (number-to-string
                   (line-number-at-pos $max))))
         ($line-format (concat "%0" (number-to-string
                                     $max-line-digit) "s:"))
         ($pattern (concat
                    "\\("
                    (mapconcat 'identity $query "\\|")
                    "\\)")))
    (save-excursion
      (swoop--mapc $q $query
        (setq $match-lines-list
              (cons (swoop--get-match-lines-list $q) $match-lines-list)))
      (swoop--mapc $l (swoop--match-lines-list-common $match-lines-list)
        (swoop--goto-line $l)
        ;; (overlay-put (make-overlay (line-beginning-position) (1+ (line-end-position)))
        ;;              'invisible 'swoop)
        (let* (($lbeg (line-beginning-position))
               ($lend (line-end-position))
               ($lov (make-overlay $lbeg $lend)))
          ;; Show lines
          (put-text-property $lbeg (min (1+ $lend) $max) 'invisible nil)
          ;; Line number overlay
          (overlay-put $lov 'before-string
                       (propertize
                        (format $line-format $l)
                        'face '(:foreground "#ff9900")))
          (overlay-put $lov 'swoop-temporary t)

          (hi-lock-set-pattern $pattern 'swoop-target-words-face)
          (with-selected-window swoop-target-window
            (hi-lock-set-pattern $pattern 'swoop-target-words-face))
          ;; (block stop
          ;;   (while (re-search-forward $pattern $lend t)
          ;;     (let* (($wbeg (match-beginning 0))
          ;;            ($wend (match-end 0))
          ;;            ($ov (make-overlay $wbeg $wend)))
          ;;       (if (eq $wbeg $wend) (cl-return-from stop nil))
          ;;       (if swoop-halt-search-process (cl-return-from stop nil))
          ;;       (overlay-put $ov 'face 'swoop-target-words-face)
          ;;       (overlay-put $ov 'swoop-temporary t)
          ;;       (with-selected-window swoop-target-window
          ;;         (setq $ov (make-overlay $wbeg $wend))
          ;;         (overlay-put $ov 'face 'swoop-target-words-face)
          ;;         (overlay-put $ov 'swoop-temporary t)
          ;;         ))))
          )))))
;; (swoop-words-overlay '("de" "sw"))

;; (swoop--get-match-lines-list "def")
(cl-defun swoop--get-match-lines-list ($query &optional $visible-only)
  (save-excursion
    (goto-char (point-min))
    (let (($pos (re-search-forward $query nil t))
          ($match-lines nil))
      (block stop
        (while $pos
          (if $visible-only
              (if (not (get-text-property (point) 'invisible))
                  (setq $match-lines (cons (line-number-at-pos) $match-lines)))
            (setq $match-lines (cons
                                (if (bolp)
                                    (1- (line-number-at-pos)) ;; end of return match
                                  (line-number-at-pos))
                                $match-lines)))
          (forward-line 1)
          (if swoop-halt-search-process (cl-return-from stop nil))
          (if (eq $pos (setq $pos (re-search-forward $query nil t)))
              (cl-return-from stop nil))))
      $match-lines)))


;; \w{2,3}.html?$
;; (swoop--pcre-convert (read-string "Input \"\\w{2,3}.html?$\" : "))
;; ^\s*\w \d{2,3}
;; (swoop--pcre-convert (read-string "Input \"^\\s*\\w \\d{2,3}\" : "))
(defsubst swoop--pcre-convert ($query)
  (nreverse
   (swoop--mapcr $q (split-string $query " " t)
     (rxt-pcre-to-elisp $q))))
;; (swoop--migemo-convert "kaki kuke")
;; (swoop--migemo-convert "kakuku")

(defvar swoop-use-pcre nil)
(defvar swoop-use-migemo nil)
(defvar swoop-migemo-options "-q -e -d /usr/local/share/migemo/utf-8/migemo-dict")
(defsubst swoop--migemo-convert ($query)
  (nreverse
   (swoop--mapcr $q (split-string $query " " t)
     (replace-regexp-in-string
      "\n" ""
      (shell-command-to-string
       (concat "cmigemo" " -w " $q " " swoop-migemo-options))))))

(defvar swoop-input-dilay 0.20)
(defvar swoop-input-threshold 2)
(defun swoop--read-from-string ($query $buf)
  (let (($timer nil))
    (unwind-protect
        (minibuffer-with-setup-hook
            (lambda ()
              (setq $timer
                    (run-with-idle-timer
                     swoop-input-dilay
                     'repeat
                     (lambda ()
                       (with-selected-window (or (active-minibuffer-window)
                                                 (minibuffer-window))
                         (let* (($content (minibuffer-contents)))
                           (when (and (not (equal swoop--minibuf-last-content $content))
                                      (or
                                       ;; When after becomeing empty
                                       (equal "" $content)
                                       ;; Avoid too many matching
                                       (>= (length $content) swoop-input-threshold)))
                             (setq swoop--minibuf-last-content $content)

                             ;; MIGEMO
                             (when swoop-use-migemo
                               (setq $content (swoop--migemo-convert $content))
                               (setq swoop-use-pcre nil))
                             ;; PCRE
                             (when swoop-use-pcre
                               (setq $content (swoop--pcre-convert $content))
                               (setq swoop-use-migemo nil))

                             (swoop-update $content $buf))))))))
          (read-from-minibuffer
           "Swoop: " (or $query "") swoop-map nil
           query-replace-from-history-variable nil t))
      (when $timer (cancel-timer $timer) (setq $timer nil))
      (setq swoop--minibuf-last-content "")
      ;; (with-current-buffer swoop-buffer
      ;;   (put-text-property (point-min) (point-max) 'invisible nil)
      ;;   (remove-text-properties (point-min) (point-max) 'swoop))
      (recenter))))

;; あいうえお　かきくけこ
;; あいうえお　さしすせそ
;; ああああああああ

;; ----------------------------------------------------------------------

(setq swoop-edit-map
  (let (($map (make-sparse-keymap)))
    ;; (define-key $map (kbd "C-x C-s") 'swoop--edit-apply-changes)
    (define-key $map (kbd "C-x C-s") 'swoop--edit-finish)
    (define-key $map (kbd "C-c C-c") 'swoop--edit-finish)
    $map))
(defvar swoop-edit-buffer "*Swoop Edit*")

(defun swoop--edit-finish ()
  (interactive)
  (select-window swoop-target-window)
  (kill-buffer swoop-edit-buffer))

(defun swoop--modify-buffer-content ($bufcont)
  (propertize
   (with-temp-buffer
     (insert $bufcont)
     ;; Embed line number infomation
     (goto-char (point-min))
     (add-text-properties
      (point) (line-end-position)
      `(swoop-line-num ,(line-number-at-pos) swoop-target t))
     (while (re-search-forward "\n" nil t)
       (add-text-properties
        (point) (line-end-position)
        `(swoop-line-num ,(line-number-at-pos) swoop-target t)))
     (buffer-substring (point-min) (point-max)))
   'face '(:height 0.9)))

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
           ($pos (point))
           ($max-line-digit
            (length (number-to-string (line-number-at-pos (buffer-end 1)))))
           $pos $eol)
      (while (setq $pos (re-search-forward
                         "^[[:space:]]*[1-9][0-9]*::[[:space:]]" nil t))
        (end-of-line)
        (setq $eol (point))
        ;; (put-text-property (line-beginning-position) $eol
        ;;                    'swoop-target
        ;;                    (swoop--set-marker (line-number-at-pos) $buf))
        ;; Make editable area
        (remove-text-properties $pos $eol '(read-only t))
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

;; ;; From replace.el
;; (define-derived-mode swoop-edit-mode fundamental-mode "Swoop-Edit"
;;   "Major mode for editing Swoop buffers.
;; In this mode, changes to the Swoop buffer are also applied to
;; the originating buffer.

;; To return to ordinary Occur mode, use \\[swoop]."
;;   (setq buffer-read-only nil)
;;   (add-hook 'after-change-functions 'swoop-after-change-function nil t)
;;   (message (substitute-command-keys
;;             "Editing: Type \\[swoop] to return to Swoop mode.")))

;; (defun swoop-cease-edit ()
;;   "Switch from Swoop Edit mode to Swoop mode."
;;   (interactive)
;;   (when (derived-mode-p 'swoop-edit-mode)
;;     (swoop-mode)
;;     (message "Switching to Swoop mode.")))

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
