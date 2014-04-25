;;; swoop-lib.el --- Peculiar buffer navigation for Emacs -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2014 by Shingo Fukuyama

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;;; Code:

(require 'cl-lib)
(require 'async)
(require 'pcre2el)
(require 'ht)

(defgroup swoop nil
  "Group for swoop"
  :prefix "swoop-" :group 'convenience)

(defvar swoop-buffer "*Swoop*")
(defvar swoop-window nil)
(defvar swoop-overlay-buffer-selection nil)
(defvar swoop-overlay-target-buffer nil)
(defvar swoop-overlay-target-buffer-selection nil)
(defvar swoop-last-selected-buffer nil)
(defvar swoop-last-selected-line nil)
(defvar swoop-buffer-info (ht-create 'equal))
(defvar swoop-minibuffer-input-dilay 0)
(defvar swoop-input-threshold 2)
(defvar swoop-minibuffer-history nil)
(defvar swoop-last-query-plain nil)
(defvar swoop-last-query-converted nil)
(defvar swoop-last-pattern nil)
(defvar swoop-minibuf-last-content nil)
(defvar swoop-parameters (ht-create 'equal)
  "To hand over current state to swoop-multi")
(defvar swoop-match-beginning-line nil)
(defvar swoop-split-denominator 3000)

(defvar swoop--target-buffer nil)
(defvar swoop--target-window nil)
(defvar swoop--target-buffer-info nil)
(defvar swoop--target-last-position nil)
(defvar swoop--target-last-line nil)

(defvar swoop-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "C-n") 'swoop-action-goto-line-next)
    (define-key map (kbd "C-p") 'swoop-action-goto-line-prev)
    (define-key map (kbd "C-g") 'swoop-action-cancel)
    (define-key map (kbd "M-<") 'swoop-action-goto-line-top)
    (define-key map (kbd "M->") 'swoop-action-goto-line-bottom)
    (define-key map (kbd "RET") 'swoop-action-goto-target-point)
    (define-key map (kbd "<C-return>") 'swoop-action-goto-target-point)
    map))

(defface swoop-face-target-line
  '((t :background "#e3e300" :foreground "#222222"))
  "Target line face"
  :group 'swoop)
(defface swoop-face-target-words
  '((t :background "#7700ff" :foreground "#ffffff"))
  "Target words face"
  :group 'swoop)
(defface swoop-face-header-format-line
  '((t :height 1.3 :foreground "#999999" :weight bold))
  "Currently selecting buffer name which appears on the header line"
  :group 'swoop)
(defface swoop-face-line-buffer-name
  '((t :height 1.5 :background "#0099cc" :foreground "#111111" :weight bold))
  "Buffer name line face"
  :group 'swoop)
(defface swoop-face-line-number
  '((t :foreground "#ff9900"))
  "Line number face"
  :group 'swoop)
(defvar swoop-n 'swoop-face-line-number
  "Abbreviate name in order to reduce async transfer size")

(defcustom swoop-use-target-magnifier: nil
  "Magnify around target line font size"
  :group 'swoop :type 'boolean)
(defcustom swoop-use-target-magnifier-around: 7
  "Magnify around target line font size"
  :group 'swoop :type 'boolean)
(defcustom swoop-use-target-magnifier-size: 1.2
  "Magnify around target line font size"
  :group 'swoop :type 'boolean)
(defcustom swoop-line-move-loop: t
  "If the selected line is at one of the edges of the list, and move further,
the selected line position will be at the other side of the list."
  :group 'swoop :type 'boolean)
(defcustom swoop-window-split-current-window: nil
 "Split window when having multiple windows open"
 :group 'swoop :type 'boolean)
(defcustom swoop-window-split-direction: 'split-window-vertically
 "Split window direction"
 :type '(choice (const :tag "vertically"   split-window-vertically)
                (const :tag "horizontally" split-window-horizontally))
 :group 'swoop)
(defcustom swoop-font-size-change: t
  "Change font size temporarily during swoop."
  :group 'swoop :type 'boolean)
(defcustom swoop-font-size: 0.9
  "Specify font size if `swoop-font-size-change:' is not nil."
  :group 'swoop :type 'number)


(defmacro swoop-mapc ($variable $list &rest $body)
  "Same as `mapc'"
  (declare (indent 2))
  (let (($list-unique (cl-gensym)))
    `(let ((,$list-unique ,$list))
       (mapc (lambda (,$variable)
               ,@$body)
             ,$list-unique))))
(defmacro swoop-mapcr ($variable $list &rest $body)
  "Same as `mapcar'"
  (declare (indent 2))
  (let (($list-unique (cl-gensym)))
    `(let ((,$list-unique ,$list)
           ($results))
       (mapc (lambda (,$variable)
               (setq $results (cons (progn ,@$body) $results)))
             ,$list-unique)
       $results)))

;; Move line up and down
(defsubst swoop-line-move-within-target-window ()
  "Manage the target window's behavior"
  (let (($line-num (get-text-property (point) 'swl))
        ($buf (get-text-property (point) 'swb)))
    (cl-labels ((line-action ()
                             (swoop-recenter)
                             (move-overlay
                              swoop-overlay-target-buffer-selection
                              (point) (min (1+ (point-at-eol)) (point-max))
                              (get-buffer $buf))
                             (if swoop-use-target-magnifier:
                                 (swoop-magnify-around-target :$buffer $buf))
                             (swoop-unveil-invisible-overlay)))
      (with-selected-window swoop--target-window
        (if (not (equal $buf swoop-last-selected-buffer))
            (progn
              (with-current-buffer $buf
                (set-window-buffer nil $buf)
                (swoop-goto-line $line-num)
                (line-action))
              (swoop-header-format-line-set $buf))
          (swoop-goto-line $line-num)
          (line-action))
        (setq swoop-last-selected-buffer $buf)))))

(defsubst swoop-action-goto-line-next ()
  (interactive)
  (swoop-line-move 'down))
(defsubst swoop-action-goto-line-prev ()
  (interactive)
  (swoop-line-move 'up))
(defsubst swoop-action-goto-line-top ()
  (interactive)
  (swoop-line-move 'top))
(defsubst swoop-action-goto-line-bottom ()
  (interactive)
  (swoop-line-move 'bottom))
(defsubst swoop-line-forward ()
  (let (($po (next-single-property-change (point) 'swl)))
    (if $po
        (if (get-text-property $po 'swl)
            (goto-char $po)
          (let (($over-header (next-single-property-change $po 'swl)))
            (if (get-text-property $over-header 'swl)
                (goto-char $over-header))))
      ;; Loop
      (if swoop-line-move-loop:
          (swoop-action-goto-line-top)))))
(defsubst swoop-line-backward ()
  (let (($po (previous-single-property-change (point) 'swl)))
    (if $po
        (if (get-text-property $po 'swl)
            (goto-char $po)
          (let (($over-header (previous-single-property-change $po 'swl)))
            (if (get-text-property $over-header 'swl)
                (goto-char $over-header))))
      (if swoop-line-move-loop:
          (swoop-action-goto-line-bottom)))))
(cl-defun swoop-line-move ($direction)
  (with-selected-window swoop-window
    (let ((current-pos (point)) is-init)
      (cl-case $direction
        (up     (swoop-line-backward))
        (down   (swoop-line-forward))
        (top    (unless (eq (point-min) (point-max))
                  (goto-char (point-min))
                  (swoop-line-forward)))
        (bottom (unless (eq (point-min) (point-max))
                  (goto-char (point-max))
                  (swoop-line-backward)))
        (init   (cond
                 ((and (bobp) (eobp))
                  (cl-return-from swoop-line-move nil))
                 ((bobp)
                  (swoop-line-forward)
                  (move-beginning-of-line 1))
                 ((eobp)
                  (swoop-line-backward)
                  (move-beginning-of-line 1))
                 (t (move-beginning-of-line 1)))
                 (setq is-init t)))
      (when (or (not (eq current-pos (point))) is-init)
        (move-overlay
         swoop-overlay-buffer-selection
         (point) (min (1+ (point-at-eol)) (point-max)))
        (swoop-line-move-within-target-window)
        (swoop-recenter)))))

;; Window configuration
(defvar swoop-display-function
  (lambda ($buf)
    (if swoop-window-split-current-window:
        (funcall swoop-window-split-direction:)
      (when (one-window-p)
        (funcall swoop-window-split-direction:)))
    (other-window 1)
    (switch-to-buffer $buf))
  "Determine how to deploy swoop window")

;; Font size manipulation
(defun swoop-overlay-font-size-change (&optional $multi)
  "Change whole buffer's text size"
  (when swoop-font-size-change:
    (let (($ov (make-overlay (point-min) (point-max))))
      (setq swoop-overlay-target-buffer (cons $ov nil))
      (overlay-put $ov 'face `(:height ,swoop-font-size:)))
    (swoop-recenter)
    (when $multi
      (swoop-mapc $b (ht-keys swoop-buffer-info)
        (unless (equal swoop--target-buffer $b)
          (with-current-buffer $b
            (let (($ov (make-overlay (point-min) (point-max))))
              (setq swoop-overlay-target-buffer
                    (cons $ov swoop-overlay-target-buffer))
              (overlay-put $ov 'face `(:height ,swoop-font-size:)))))))))

(defvar swoop-overlay-magnify-around-target-line nil)
(cl-defun swoop-magnify-around-target
    (&key ($around swoop-use-target-magnifier-around:)
          ($size swoop-use-target-magnifier-size:)
          $delete $buffer)
  "Magnify lines around the target line."
  (with-selected-window swoop--target-window
    (cond ((not swoop-overlay-magnify-around-target-line)
           (setq swoop-overlay-magnify-around-target-line
                 (make-overlay
                  (point-at-bol (- 0 $around))
                  (point-at-bol $around)))
           (overlay-put swoop-overlay-magnify-around-target-line
                        'face `(:height ,$size))
           (overlay-put swoop-overlay-magnify-around-target-line
                        'priority 100))
          ((and $delete swoop-overlay-magnify-around-target-line)
           (delete-overlay swoop-overlay-magnify-around-target-line))
          (t (move-overlay
              swoop-overlay-magnify-around-target-line
              (point-at-bol (- 0 $around))
              (point-at-bol $around)
              (get-buffer $buffer))))))

(defsubst swoop-goto-line ($line)
  (goto-char (point-min))
  (forward-line (1- $line)))

(defsubst swoop-recenter ()
  (recenter (/ (window-height) 2)))

(defsubst swoop-boblp (&optional $point)
  (save-excursion
    (goto-char (point-min))
    (eq (line-number-at-pos)
        (progn (goto-char (or $point (point))) (line-number-at-pos)))))

(defsubst swoop-eoblp (&optional $point)
  (save-excursion
    (goto-char (point-max))
    (eq (line-number-at-pos)
        (progn (goto-char (or $point (point))) (line-number-at-pos)))))

(defun swoop-header-format-line-set ($buffer-name)
  (when (stringp $buffer-name)
    (with-selected-window swoop-window
      (setq header-line-format
            (propertize $buffer-name 'face 'swoop-face-header-format-line)))))

;; Converter
;; \w{2,3}.html?$
;; (swoop-pcre-convert (read-string "PCRE: " "\\w{2,3}.html?$"))
;; ^\s*\w \d{2,3}
;; (swoop-pcre-convert (read-string "PCRE: " "^\\s*\\w \\d{2,3}"))
(defvar swoop-use-pcre nil)
(defsubst swoop-pcre-convert ($query)
  (nreverse
   (swoop-mapcr $q (split-string $query " " t)
     (rxt-pcre-to-elisp $q))))

;; (swoop-migemo-convert "kaki kuke")
;; (swoop-migemo-convert "kakuku")
(defvar swoop-use-migemo nil)
(defvar swoop-migemo-options
  "-q -e -d /usr/local/share/migemo/utf-8/migemo-dict")
(defsubst swoop-migemo-convert ($query)
  (if (executable-find "cmigemo")
      (nreverse
       (swoop-mapcr $q (split-string $query " " t)
         (replace-regexp-in-string
          "\n" ""
          (shell-command-to-string
           (concat "cmigemo" " -w " $q " " swoop-migemo-options)))))
    (error "cmigemo not found...")))

(defun swoop-convert-input ($input)
  (cond
   ;; PCRE
   ((and swoop-use-pcre
         (not swoop-use-migemo))
    (setq $input (swoop-pcre-convert $input)))
   ;; MIGEMO
   ((and swoop-use-migemo
         (not swoop-use-pcre))
    (setq $input (swoop-migemo-convert $input)))
   (t
    (if (string-match "^\\\\b$" $input)    (setq $input nil))
    (if (string-match "[^\\]\\\\$" $input) (setq $input nil))
    (if (string-match "\\[[^\]]*$" $input) (setq $input nil))))
  $input)

;; Unveil a hidden target block of lines
(defvar swoop-invisible-targets nil)
(defsubst swoop-restore-unveiled-overlay ()
  (when swoop-invisible-targets
    (swoop-mapc $ov swoop-invisible-targets
      (overlay-put (car $ov) 'invisible (cdr $ov)))
    (setq swoop-invisible-targets nil)))
(defsubst swoop-unveil-invisible-overlay ()
  "Show hidden text temporarily to view it during swoop.
This function needs to call after latest
swoop-overlay-target-buffer-selection moved."
  (swoop-restore-unveiled-overlay)
  (swoop-mapc $ov
      (overlays-in (overlay-start swoop-overlay-target-buffer-selection)
                   (overlay-end swoop-overlay-target-buffer-selection))
    (let (($type (overlay-get $ov 'invisible)))
      (when $type
        (overlay-put $ov 'invisible nil)
        (setq swoop-invisible-targets
              (cons (cons $ov $type) swoop-invisible-targets))))))

(defun swoop-set-buffer-info ($buf)
  "Collect buffers information. It's used for multiple uses."
  (with-current-buffer $buf
    (let* (($buf-content    (buffer-substring-no-properties
                             (point-min) (point-max)))
           ($point          (point))
           ($point-min      (point-min))
           ($point-max      (point-max))
           ($max-line       (line-number-at-pos $point-max))
           ($max-line-digit (length (number-to-string $max-line)))
           ($line-format    (concat "%0"
                                    (number-to-string $max-line-digit)
                                    "s: "))
           ($by swoop-split-denominator)   ; Buffer divide by
           ($result  (/ $max-line $by))    ; Result of division
           ($rest    (% $max-line $by))    ; Rest of division
           ;; Number of divided parts of a buffer
           ($buf-num (if (eq 0 $rest) $result (1+ $result)))
           ($separated-buffer))
      (let (($with-end-break (concat $buf-content "\n")))
        (cl-dotimes ($i $buf-num)
          (setq $separated-buffer
                (cons
                 (substring-no-properties
                  $with-end-break
                  (1- (save-excursion (swoop-goto-line (1+ (* $i $by))) (point)))
                  (if (>= (* (1+ $i) $by) $max-line)
                      nil
                    (1- (save-excursion
                          (swoop-goto-line (1+ (* (1+ $i) $by))) (point)))))
                 $separated-buffer))))
      (setq swoop--target-buffer-info
            (ht ("buf-separated" (nreverse $separated-buffer))
                ("buf-number"              $buf-num)
                ("point"                   $point)
                ("point-min"               $point-min)
                ("point-max"               $point-max)
                ("max-line"                $max-line)
                ("max-line-digit"          $max-line-digit)
                ("line-format"             $line-format)
                ("divide-by"               $by)))
      (ht-set swoop-buffer-info $buf swoop--target-buffer-info)))
  nil)

(defvar swoop-multi-ignore-buffers-match "^\\*"
  "Regexp to eliminate buffers you don't want to see")
(defun swoop-multi-get-buffer-list ()
  (let ($buflist1 $buflist2)
    ;; eliminate buffers start with whitespace and dired buffers
    (mapc (lambda ($buf)
            (setq $buf (buffer-name $buf))
            (unless (string-match "^\\s-" $buf)
              (unless (eq 'dired-mode (with-current-buffer $buf major-mode))
                (setq $buflist1 (cons $buf $buflist1)))))
          (buffer-list))
    ;; eliminate buffers match pattern
    (mapc (lambda ($buf)
            (unless (string-match
                     swoop-multi-ignore-buffers-match
                     $buf)
              (setq $buflist2 (cons $buf $buflist2))))
          $buflist1)
    $buflist2))

(defun swoop-set-buffer-info-all ()
  (let (($bufs (swoop-multi-get-buffer-list)))
    (swoop-mapc $buf $bufs
      (if (member $buf (ht-keys swoop-buffer-info))
          (if (with-current-buffer $buf (buffer-modified-p))
              (swoop-set-buffer-info $buf))
        (swoop-set-buffer-info $buf)))
    (swoop-mapc $buf (ht-keys swoop-buffer-info)
      (unless (member $buf $bufs)
        (ht-remove! swoop-buffer-info $buf)))))

(defun swoop-buffer-info-get ($buf $key2)
  (ht-get (ht-get swoop-buffer-info $buf) $key2))

(defun swoop-buffer-info-get-map ($key)
  (ht-map (lambda (ignored $binfo)
            (ht-get $binfo $key))
          swoop-buffer-info))

;; (swoop-nearest-line 50 '(10 90 20 80 30 40 45 56 70))
(defun swoop-nearest-line ($target $list)
  "Return the nearest number of $target out of $list."
  (when (and $target $list)
    (let ($result)
      (cl-labels ((filter ($fn $elem $list)
                          (let ($r)
                            (mapc (lambda ($e)
                                    (if (funcall $fn $elem $e)
                                        (setq $r (cons $e $r))))
                                  $list) $r)))
        (if (eq 1 (length $list))
            (setq $result (car $list))
          (let* (($lts (filter '> $target $list))
                 ($gts (filter '< $target $list))
                 ($lt (if $lts (apply 'max $lts)))
                 ($gt (if $gts (apply 'min $gts)))
                 ($ltg (if $lt (- $target $lt)))
                 ($gtg (if $gt (- $gt $target))))
            (setq $result
                  (cond ((memq $target $list) $target)
                        ((and (not $lt) (not $gt)) nil)
                        ((not $gtg) $lt)
                        ((not $ltg) $gt)
                        ((eq $ltg $gtg) $gt)
                        ((< $ltg $gtg) $lt)
                        ((> $ltg $gtg) $gt)
                        (t 1))))))
      $result)))


;; Async
(defvar swoop-async-pool (make-hash-table :test 'equal))
(defvar swoop-async-id-latest nil)
(defvar swoop-async-id-last nil)
(defvar swoop-async-get-match-lines-list
  "Byte compiled function. It works in async process.")

(defsubst swoop-async-old-session? ()
  (not (equal swoop-async-id-last swoop-async-id-latest)))

(defmacro swoop-async-start ($start-func $finish-func)
  (require 'find-func)
  (let ((procvar (make-symbol "proc")))
    `(let* ((sexp ,$start-func)
            (,procvar
             (swoop-async-start-process
              "swoop-batch" (file-truename
                       (expand-file-name invocation-name
                                         invocation-directory))
              ,$finish-func
              "-Q" "-l" ,(symbol-file 'async-batch-invoke 'defun)
              "-batch" "-f" "async-batch-invoke"
              (if async-send-over-pipe
                  "<none>"
                (with-temp-buffer
                  (async--insert-sexp (list 'quote sexp))
                  (buffer-string))))))
       (if async-send-over-pipe
           (async--transmit-sexp ,procvar (list 'quote sexp)))
       ,procvar)))

(defun swoop-async-start-process (name program finish-func &rest program-args)
  (let* ((buf (generate-new-buffer (concat "*" name "*")))
         (proc (let ((process-connection-type nil))
                 (apply #'start-process name buf program program-args))))
    (with-current-buffer buf
      (set (make-local-variable 'async-callback) finish-func)
      (set-process-sentinel proc #'async-when-done)
      (unless (string= name "swoop-batch")
        (set (make-local-variable 'async-callback-for-process) t))
      proc)))

(defun swoop-async-kill-process-buffer ()
  (mapc (lambda ($buf)
          (setq $buf (buffer-name $buf))
          (when (string-match "^\\*swoop-batch\\*" $buf)
            (let ((kill-buffer-query-functions nil))
              (kill-buffer $buf))))
        (buffer-list)))

(defun swoop-async-kill-process ()
  (mapc (lambda ($proc)
          (when (string-match "swoop-batch" (process-name $proc))
            (delete-process $proc)))
        (process-list)))

(defun swoop-async-get-match-lines-list
  ($query $from $line-format $line-face $buf &optional $pre-select $match-beginning)
  "Distributed processing by async.el"
  ;; Prevent "Odd length text property list" error
  (setq vc-handled-backends nil)
  (save-excursion
    (let* (($lines nil)
           ($pos-min (point-min))
           ($pos-max (point-max))
           (buffer-invisibility-spec nil)
           ($match-lines)
           ($match-total
            (if $pre-select
                (let (($max-line (line-number-at-pos $pos-max)))
                  (cons
                   (sort (delq nil (mapcar (lambda ($n)
                                             (if (and (> $n $from)
                                                      (<= $n (+ $max-line $from)))
                                                 (- $n $from)))
                                           $pre-select))
                         '>)
                   nil))))
           ($match-lines-common)
           ($match-position-fn
            (if $match-beginning
                (lambda () (line-number-at-pos (match-beginning 0)))
              (lambda () (line-number-at-pos)))))
      (goto-char $pos-min)
      (put-text-property $pos-min $pos-max 'swb $buf)
      ;; Get lines at least one match
      (mapc (lambda ($q)
              (save-excursion
                (goto-char $pos-min)
                (while (re-search-forward $q nil t)
                  (setq $match-lines (cons (funcall $match-position-fn)
                                           $match-lines))
                  (forward-line))
                (setq $match-total (cons $match-lines $match-total))
                (setq $match-lines nil)))
            $query)
      ;; Common match line mapping
      (let* (($results)
             ($length (length $match-total)))
        (when (> $length 0)
          (setq $results (car-safe $match-total))
          (if (> $length 1)
              (mapc (lambda ($l)
                      (setq $results
                            (let (($r) ($nth 0))
                              (while $results
                                (let (($top (car $results)))
                                  (when (memq $top $l)
                                    (setq $r (cons $top $r))))
                                (setq $nth (1+ $nth))
                                (setq $results (cdr $results)))
                              (nreverse $r))))
                    (cdr $match-total))))
        (setq $match-lines-common $results))
      ;; Culling all words match lines
      (mapc (lambda ($l)
              (goto-char $pos-min)
              (forward-line (1- $l))
              (let (($line-num (+ $l $from)))
                (setq $lines
                      (cons
                       (propertize
                        (buffer-substring (point) (1+ (point-at-eol)))
                        'line-prefix
                        (propertize
                         (format $line-format $line-num)
                         'face $line-face)
                        'swl $line-num)
                       $lines))))
            $match-lines-common)
      (setq $match-lines-common
            (mapcar (lambda ($ln) (+ $ln $from)) $match-lines-common))
      (cons $match-lines-common $lines))))
(setq swoop-async-get-match-lines-list
      (byte-compile 'swoop-async-get-match-lines-list))


(cl-defun swoop-overlay-word ($pattern $buf)
  "Overlay match words."
  (with-current-buffer $buf
    (save-excursion
      (goto-char (point-min))
      (overlay-recenter (point-max))
      (while (re-search-forward $pattern nil t)
        (if (swoop-async-old-session?) (cl-return-from stop1))
        (let* (($beg (match-beginning 0))
               ($end (match-end 0))
               ($ov (make-overlay $beg $end)))
          (if (eq $beg $end) (cl-return-from swoop-overlay-word))
          (overlay-put $ov 'face 'swoop-face-target-words)
          (overlay-put $ov 'swoop-temporary t)
          (overlay-put $ov 'priority 20))))))


(provide 'swoop-lib)
;;; swoop-lib.el ends here
