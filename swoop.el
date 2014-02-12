(require 'pcre2el)
(require 'dash)
;; (my-nest-specific-text-property-position 'invisible 'swoop)
;; buffer-invisibility-spec

(defgroup swoop nil
  "Group for swoop"
  :prefix "swoop-" :group 'convenience)

(defvar swoop-buffer nil)
(defvar swoop-window nil)
(defvar swoop-target-buffer nil)
(defvar swoop-target-window nil)
(defvar swoop-target-overlay nil)
(defvar swoop-target-overlay-within-target-window nil)

(defvar swoop-exit-status 0)

(defvar swoop--cached-count 0)
(defvar swoop--minibuf-last-content "")
(defvar swoop--last-query "")
(setq swoop-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "C-n") 'swoop--next-line)
    (define-key map (kbd "C-p") 'swoop--prev-line)
    (define-key map (kbd "C-c C-e") 'swoop--edit)
    (define-key map (kbd "RET") 'swoop--default-action)
    map))



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
         (recenter)))) (with-current-buffer swoop-target-buffer (point)))
  (exit-minibuffer))



(defvar swoop-invisible-targets nil)
(defsubst swoop--restore-unveiled-overlay ()
  (when swoop-invisible-targets
    (mapc (lambda ($ov) (overlay-put (car $ov) 'invisible (cdr $ov)))
          swoop-invisible-targets)
    (setq swoop-invisible-targets nil)))
(defsubst swoop--unveil-invisible-overlay ()
  "Show hidden text temporarily to view it during swoop.
This function needs to call after latest swoop-target-overlay-within-target-window moved."
  (swoop--restore-unveiled-overlay)
  (mapc (lambda ($ov)
          (let (($type (overlay-get $ov 'invisible)))
            (when $type
              (overlay-put $ov 'invisible nil)
              (setq swoop-invisible-targets
                    (cons (cons $ov $type) swoop-invisible-targets)))))
        (overlays-in (overlay-start swoop-target-overlay-within-target-window)
                     (overlay-end swoop-target-overlay-within-target-window))))
(defsubst swoop--goto-line ($line)
  (goto-char (point-min))
  (unless (re-search-forward "\n" nil t (1- $line))
    (goto-char (point-max))))
(defsubst swoop--move-line-within-target-window ()
  (let (($line-num (line-number-at-pos)))
    (with-selected-window swoop-target-window
      (swoop--goto-line $line-num)
      (move-overlay
       swoop-target-overlay-within-target-window
       (point) (min (1+ (line-end-position)) (point-max)))
      (swoop--unveil-invisible-overlay)
      (recenter))))
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
       swoop-target-overlay
       (point) (min (1+ (line-end-position)) (point-max)))
      (unless (or (bobp) (eobp))
        (swoop--move-line-within-target-window))
      (recenter))))
(defsubst swoop--forward-visible-line (&optional $recur)
  (unless (and (bobp) (eobp))
    (if (not (eobp))
        (let ((buffer-invisibility-spec '(t))
              $pos)
          (while (progn (re-search-forward "\n" nil t)
                        (goto-char (line-end-position))
                        (and (eq 'swoop (get-text-property (point) 'invisible))
                             (not (eobp)))))
          (goto-char (line-beginning-position))))
    (if (and (eobp) (not (bobp)) (not $recur))
        (swoop--backward-visible-line t))))
(defsubst swoop--backward-visible-line (&optional $recur)
  (unless (and (bobp) (eobp))
    (if (not (bobp))
        (let ((buffer-invisibility-spec '(t))
              $pos)
          (while (progn (re-search-backward "\n" nil t)
                        (goto-char (line-beginning-position))
                        (and (eq 'swoop (get-text-property (point) 'invisible))
                             (not (bobp)))))))
    (if (and (bobp) (not (eobp)) (not $recur))
        (swoop--forward-visible-line t))))
(defsubst swoop--next-line () (interactive)
  (swoop--move-line 'down))
(defsubst swoop--prev-line () (interactive)
  (swoop--move-line 'up))
(defsubst swoop--invisible-on ()
  (add-to-invisibility-spec 'swoop))
(defsubst swoop--invisible-off ()
  (remove-from-invisibility-spec 'swoop))

(global-set-key (kbd "C-o") 'swoop)
(global-set-key (kbd "M-o") 'swoop-pcre-regexp)
(defvar swoop-point-at-function (lambda () (thing-at-point 'symbol)))
(defvar swoop-target-buffer-overlay nil)
(cl-defun swoop--core (&key $query)
  (setq swoop-target-buffer-overlay (make-overlay (point-min) (point-max)))
  (overlay-put swoop-target-buffer-overlay 'face '(:height 0.9))
  (setq swoop-target-overlay-within-target-window
        (make-overlay (point-min) (point-min)))
  (overlay-put swoop-target-overlay-within-target-window
               'face 'swoop-target-line-face)
  (save-window-excursion
    (let* (($pos (point))
           ($pos-min (point-min))
           ($bufcont (buffer-substring-no-properties $pos-min (point-max))
                     ;;(buffer-substring $pos-min (point-max))
                     ))
      (setq swoop-target-buffer (current-buffer))
      (setq swoop-target-window (get-buffer-window swoop-target-buffer))
      (setq swoop-target-overlay (make-overlay $pos-min $pos-min))

      (setq swoop-buffer "ttt")
      (pop-to-buffer swoop-buffer)
      (erase-buffer)
      (setq $bufcont (swoop--modify-buffer-content $bufcont))
      (insert $bufcont)
      ;; (linum-mode 1)
      ;;(swoop-line-number-overlay)
      (setq swoop-window (get-buffer-window swoop-buffer))
      (goto-char $pos))
    (let ((buffer-invisibility-spec '(t))) ;; temporary unvail text for org-mode, or etc
      (unwind-protect
          (progn
            (overlay-put swoop-target-overlay 'face 'swoop-target-line-face)
            (overlay-put swoop-target-overlay 'priority 15)
            ;;(setq swoop--minibuf-last-content $query)
            (when $query
              (setq swoop--minibuf-last-content $query)
              (swoop-update
               (split-string $query " " t) swoop-buffer))
            (swoop--read-from-string $query swoop-buffer))
        (swoop--clear-overlay)
        (swoop--invisible-off)
        (kill-buffer swoop-buffer)
        ;;(exit-minibuffer)
        )
      )))
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
  (swoop--core :$query (or $query (swoop--pre-input))))
(defun swoop-pcre-regexp (&optional $query)
  (interactive)
  (let ((swoop-use-pcre t))
    (swoop--core :$query (or $query (swoop--pre-input)))))
(defun swoop-migemo (&optional $query)
  (interactive)
  (let ((swoop-use-migemo t))
    (swoop--core :$query (or $query (swoop--pre-input)))))

(defsubst swoop--clear-overlay () (interactive)
  (cl-flet ((swoop-clear-overlay
             ($buf)
             (with-current-buffer $buf
               (mapc (lambda ($ov)
                       (when (overlay-get $ov 'swoop-temporary)
                         (delete-overlay $ov)))
                     (overlays-in (point-min) (point-max))))))
  (swoop-clear-overlay swoop-buffer)
  (swoop-clear-overlay swoop-target-buffer)
  (delete-overlay swoop-target-buffer-overlay)
  (delete-overlay swoop-target-overlay)
  (delete-overlay swoop-target-overlay-within-target-window)))

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
                (swoop--clear-overlay)
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
                    (swoop--forward-visible-line t))
                (if (eobp)
                    (swoop--backward-visible-line t))))
            ;; (with-selected-window swoop-target-window
              ;; (swoop-words-overlay $query)
              ;; )
            )))
    ))


(setq swoop--last-visible-lines nil)
(defun swoop--match-lines-list-common ($match-lines-list)
  "Return common numbers list of several numbers lists.
'((1 2 3 4 5) (2 3 4 8) (2 3 9)) -> '(2 3)"
  (let* (($list $match-lines-list)
         ($results)
         ($length (length $list)))
    (when (> $length 0)
      (setq $results (car-safe $list))
      (if (> $length 1)
          (dolist ($l (cdr $list))
            (setq $results (-intersection $results $l)))))
    (setq swoop--last-visible-lines $results)))


(cl-defun swoop-words-overlay ($query)
  (let (($match-lines-list nil))
    (save-excursion
      (dolist ($q $query)
        (setq $match-lines-list
              (cons (swoop--get-match-lines-list $q) $match-lines-list)))
      (dolist ($l (swoop--match-lines-list-common $match-lines-list))
        (swoop--goto-line $l)
        ;; (overlay-put (make-overlay (line-beginning-position) (1+ (line-end-position)))
        ;;              'invisible 'swoop)
        (let* (($lbeg (line-beginning-position))
               ($lend (line-end-position))
               ($pattern (concat
                          "\\("
                          (mapconcat 'identity $query "\\|")
                          "\\)"))
               ($lov (make-overlay $lbeg $lend)))
          ;; Show lines
          (put-text-property $lbeg (1+ $lend) 'invisible nil)
          ;; Line number overlay
          ;; (overlay-put $lov 'before-string
          ;;              (propertize
          ;;               (format "%03s:" $l)
          ;;               'face '(:foreground "#ff9900")))
          ;; (overlay-put $lov 'swoop-temporary t)
          ;; (overlay-put $lov 'priority 10)
          (block stop
            (while (re-search-forward $pattern $lend t)
              (let* (($wbeg (match-beginning 0))
                     ($wend (match-end 0))
                     ($ov (make-overlay $wbeg $wend)))
                (if swoop-halt-search-process (cl-return-from stop nil))
                (if (eq $wbeg $wend) (cl-return-from stop nil))
                (overlay-put $ov 'face 'swoop-target-words-face)
                (overlay-put $ov 'swoop-temporary t)
                (overlay-put $ov 'priority 20)
                (with-selected-window swoop-target-window
                  (setq $ov (make-overlay $wbeg $wend))
                  (overlay-put $ov 'face 'swoop-target-words-face)
                  (overlay-put $ov 'swoop-temporary t)
                  (overlay-put $ov 'priority 20))
                  ))))))))
;; (swoop-words-overlay '("def" "match"))


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
            (setq $match-lines (cons (line-number-at-pos) $match-lines)))
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
  (let ($results)
    (mapc (lambda ($q)
            (setq $results (cons (rxt-pcre-to-elisp $q) $results)))
          (split-string $query " " t))
    (nreverse $results)))

;; (swoop--migemo-convert "kaki kuke")
;; (swoop--migemo-convert "kakuku")

(defvar swoop-use-pcre nil)
(defvar swoop-use-migemo nil)
(defvar swoop-migemo-options "-q -e -d /usr/local/share/migemo/utf-8/migemo-dict")
(defsubst swoop--migemo-convert ($query)
  (let ($results)
    (mapc (lambda ($q)
            (setq $results
                  (cons
                   (replace-regexp-in-string
                    "\n" ""
                    (shell-command-to-string
                     (concat "cmigemo" " -w " $q " " swoop-migemo-options)))
                   $results)))
          (split-string $query " " t))
    (nreverse $results)))

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
                                      (>= (length $content) swoop-input-threshold))
                             (setq swoop--minibuf-last-content $content)

                             ;; MIGEMO
                             (when swoop-use-migemo
                               (setq $content (swoop--migemo-convert $content))
                               (setq swoop-use-pcre nil))
                             ;; PCRE
                             (when swoop-use-pcre
                               (setq $content (swoop--pcre-convert $content))
                               (setq swoop-use-migemo nil))

                             (swoop-update $content $buf)))
                         )))))
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
        (define-key $map (kbd "C-x C-s") 'swoop--edit-apply-changes)
        $map))

(defun swoop--modify-buffer-content ($bufcont)
  (propertize
   (with-temp-buffer
     (insert $bufcont)
     ;; Embed line number infomation
     (goto-char (point-min))
     (add-text-properties
      (point) (line-end-position)
      `(swoop-line-num ,(line-number-at-pos)
                       swoop-target t))
     (while (re-search-forward "\n" nil t)
       (add-text-properties
        (point) (line-end-position)
        `(swoop-line-num ,(line-number-at-pos)
                         swoop-target t)))
     (buffer-substring (point-min) (point-max)))
   ;;$bufcont
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
        (put-text-property (line-beginning-position) $eol
                           'swoop-target
                           (swoop--set-marker (line-number-at-pos) $buf))
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
           (length (number-to-string (line-number-at-pos (buffer-end 1))))))
      (save-excursion
        (dolist ($l $visible-lines)
          (swoop--goto-line $l)
          (setq $results
                (cons (cons (propertize
                             (format (concat "%0" (number-to-string $max-line-digit)
                                             "s:: ") $l)
                             'face 'swoop-line-number-face
                             'swoop-prefix t
                             'intangible t
                             'rear-nonsticky t)
                            (buffer-substring
                             (line-beginning-position) (line-end-position)))
                      $results))))
      $results)))
;;(swoop--get-match-line-content swoop--last-visible-lines)
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
       (pop-to-buffer "swoop for edit")
       (erase-buffer)
       (use-local-map swoop-edit-map)
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
       (re-search-forward "^[[:space:]]*\\([0-9]+\\)::[[:space:]]" nil t)
       )
     ;; Args
     $bufcont
     (buffer-name swoop-target-buffer)
     swoop-target-window
     swoop--last-visible-lines)
    (exit-minibuffer)))

(defun swoop--edit-apply-changes ()
  (let (($results)
        ($edit-buffer (current-buffer)))
    ;; (let ((inhibit-read-only t))
      ;; (remove-text-properties (point-min) (point-max) '(read-only t))
      ;; (set-text-properties (point-min) (point-max) nil)
      (goto-char (point-min))
      (while (re-search-forward "^[[:space:]]*\\([0-9]+\\)::[[:space:]]" nil t)
        (setq $results
              (cons (cons (string-to-number (match-string 1))
                          (buffer-substring-no-properties
                           (point)
                           (save-excursion
                             (if (re-search-forward
                                  "^[[:space:]]*[0-9]+::[[:space:]]" nil t)
                                 (1- (match-beginning 0))
                               (goto-char (point-max))
                               (re-search-backward "\n" nil t)))))
                    $results)))
      (with-current-buffer swoop-target-buffer
        (save-excursion
          (let ((buffer-invisibility-spec '(t)))
            (loop for ($k . $v) in $results
                  do (progn
                       (swoop--goto-line $k)
                       (delete-region (line-beginning-position)
                                      (line-end-position))
                       (insert $v))))))
    (select-window swoop-target-window)
    (kill-buffer $edit-buffer)))



(provide 'swoop)
