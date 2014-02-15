;;; swoop.el --- Peculiar buffer navigation for Emacs -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2014 by Shingo Fukuyama

;; Version: Developing
;; Author: Shingo Fukuyama - http://fukuyama.co
;; URL: https://github.com/ShingoFukuyama/swoop
;; Created: Feb 14 2014
;; Keywords: swoop inner buffer search navigation
;; Package-Requires: ((pcre2el "1.5") (async "1.1") (emacs "24"))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;;; Commentary:

;; Developing...

;; Example config
;; ----------------------------------------------------------------
;; ;; Require   async.el  https://github.com/jwiegley/emacs-async
;; ;;         pcre2el.el  https://github.com/joddie/pcre2el
;; (require 'swoop)
;; (global-set-key (kbd "C-o")   'swoop)
;; (global-set-key (kbd "M-o")   'swoop-pcre-regexp)
;; (global-set-key (kbd "C-S-o") 'swoop-back-to-last-position)
;; (global-set-key (kbd "H-6")   'swoop-migemo)

;; ;; Swoop Edit Mode
;; ;; During swoop, press [C-c C-e]
;; ;; You can edit synchronously

;;; TODO
;; Unpropertize (thing-at-point 'symbol)
;; Prevent long time loop words (], \\b, {0,} ...)

;;; Code:

(require 'async)
(require 'pcre2el)

(defgroup swoop nil
  "Group for swoop"
  :prefix "swoop-" :group 'convenience)

(defvar swoop-buffer "*Swoop*")
(defvar swoop-window nil)
(defvar swoop-buffer-selection-overlay nil)
(defvar swoop-target-buffer-overlay nil)
(defvar swoop-target-buffer-selection-overlay nil)

(defvar swoop--target-buffer)
(defvar swoop--target-window)
(defvar swoop--last-position)
(defvar swoop--minibuf-last-content)
(defvar swoop--last-query-plain)
(defvar swoop--last-query-converted)
(defvar swoop--buffer-content)

(defvar swoop-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "C-n") 'swoop--next-line)
    (define-key map (kbd "C-p") 'swoop--prev-line)
    (define-key map (kbd "C-g") 'swoop--cancel)
    (define-key map (kbd "RET") 'swoop--default-action)
    (define-key map (kbd "<C-return>") 'swoop--default-action)
    map))

;; Face
(defface swoop-target-line-face
  '((t :background "#e3e300" :foreground "#222222"))
  "Target line face for swoop"
  :group 'swoop)
(defface swoop-target-words-face
  '((t :background "#7700ff" :foreground "#ffffff"))
  "Target words face for swoop"
  :group 'swoop)
(defface swoop-line-number-face
  '((t :foreground "#ff9900"))
  "Line number face for swoop"
  :group 'swoop)

;; Macro
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

;; Font size change
(defcustom swoop-font-size-change: 0.9
  "Change fontsize temporarily during swoop."
  :group 'swoop :type 'number)
(defcustom swoop-use-target-magnifier: nil
  "Magnify around target line font size"
  :group 'swoop :type 'boolean)
(defvar swoop-magnify-around-target-overlay nil)
(cl-defun swoop--magnify-around-target (&key ($around 10) ($size 1.2) $delete)
  (with-selected-window swoop--target-window
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

;; Window configuration
(defcustom swoop-window-split-current-window nil
 "Split window when having multiple windows open"
 :group 'swoop :type 'boolean)
(defcustom swoop-window-split-direction 'split-window-vertically
 "Split window direction"
 :type '(choice (const :tag "vertically"   split-window-vertically)
                (const :tag "horizontally" split-window-horizontally))
 :group 'swoop)
(defvar swoop-display-function
  (lambda ($buf)
    (if swoop-window-split-current-window
        (funcall swoop-window-split-direction)
      (when (one-window-p)
        (funcall swoop-window-split-direction)))
    (other-window 1)
    (switch-to-buffer $buf)))

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
    (with-selected-window swoop--target-window
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

;; Default action
(defun swoop--default-action ()
  (interactive)
  (run-with-timer
   0 nil
   (lambda ($po)
     (with-selected-window swoop--target-window
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
   (with-current-buffer swoop--target-buffer (point)))
  (exit-minibuffer))

;; Unveil a hidden target block of lines
(defvar swoop-invisible-targets nil)
(defsubst swoop--restore-unveiled-overlay ()
  (when swoop-invisible-targets
    (swoop--mapc $ov swoop-invisible-targets
      (overlay-put (car $ov) 'invisible (cdr $ov)))
    (setq swoop-invisible-targets nil)))
(defsubst swoop--unveil-invisible-overlay ()
  "Show hidden text temporarily to view it during swoop.
This function needs to call after latest
swoop-target-buffer-selection-overlay moved."
  (swoop--restore-unveiled-overlay)
  (swoop--mapc $ov
      (overlays-in (overlay-start swoop-target-buffer-selection-overlay)
                   (overlay-end swoop-target-buffer-selection-overlay))
    (let (($type (overlay-get $ov 'invisible)))
      (when $type
        (overlay-put $ov 'invisible nil)
        (setq swoop-invisible-targets
              (cons (cons $ov $type) swoop-invisible-targets))))))

;; Move line up and down
(defsubst swoop--goto-line ($line)
  (goto-char (point-min))
  (unless (re-search-forward "\n" nil t (1- $line))
    (goto-char (point-max))))
(defsubst swoop--move-line-within-target-window ($line-num)
  (with-selected-window swoop--target-window
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
                     (setq $pos (re-search-forward "\n.+$" nil t))
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
                     (setq $pos (re-search-backward "\n" nil t))
                     (and (eq 'swoop (get-text-property $pos 'invisible))
                          (not (swoop--boblp $pos))))))
          (when (not (eq 'swoop (get-text-property $pos 'invisible)))
            (goto-char $pos)
            (goto-char (line-beginning-position)))))))
(cl-defsubst swoop--move-line ($direction)
  (with-selected-window swoop-window
    (let ($line-num)
      (cl-case $direction
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

(setq swoop-invisible-tag 'swoop)
(setq swoop-invisible-tag-last nil)
;; For update matched lines
(defsubst swoop--invisible-on ()
  (add-to-invisibility-spec swoop-invisible-tag))
(defsubst swoop--invisible-off ()
  (remove-from-invisibility-spec swoop-invisible-tag))

;; Overlay
(cl-defun swoop--clear-overlay (&key $to-empty $kill)
  (swoop--mapc $buf (list swoop--target-buffer swoop-buffer)
    (if (swoop--old-session?) (cl-return))
    (when (get-buffer swoop-buffer)
      (with-current-buffer $buf
        (swoop--mapc $ov (overlays-in (point-min) (point-max))
          (if (swoop--old-session?) (cl-return))
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

(defvar swoop--async-pool (make-hash-table :test 'equal))
(defvar swoop--async-id-latest nil)
(cl-defun swoop--core (&key $query $resume)
  (setq
   swoop--last-position (point)
   swoop--target-buffer  (current-buffer)
   swoop--target-window  (get-buffer-window swoop--target-buffer)
   swoop--buffer-content (buffer-substring-no-properties
                         (point-min) (point-max)))
  ;; Font size change
  (setq swoop-target-buffer-overlay (make-overlay (point-min) (point-max)))
  (overlay-put swoop-target-buffer-overlay
               'face `(:height ,swoop-font-size-change:))
  (recenter)
  (swoop--target-buffer-selection-overlay-set)
  (save-window-excursion
    (let* (($pos-min (point-min))
           ($bufcont swoop--buffer-content)
           ($bufname swoop--target-buffer)
           ($bufwin swoop--target-window)
           ($po swoop--last-position))
      (funcall swoop-display-function swoop-buffer)
      (erase-buffer)
      (setq swoop-window (get-buffer-window swoop-buffer))
      (set (make-local-variable 'swoop--buffer-content) $bufcont)
      (set (make-local-variable 'swoop--target-buffer)  $bufname)
      (set (make-local-variable 'swoop--target-window)  $bufwin)
      (set (make-local-variable 'swoop--last-position)  $po)
      (insert (swoop--modify-buffer-content $bufcont))
      (goto-char $po)
      (swoop--buffer-selection-overlay-set))
    ;; Temporary unvail text for org-mode, or etc
    (let ((buffer-invisibility-spec '(t)))
      (unwind-protect
          (when (get-buffer swoop-buffer)
            (clrhash swoop--async-pool)
            (when (or $query $resume)
              ;; Prevent following minibuffer session
              (setq swoop--minibuf-last-content $query)
              ;; First time
              (if (or (listp $query) $resume)
                  (swoop-update swoop--last-query-converted swoop-buffer)
                (swoop-update (split-string $query " " t) swoop-buffer)))
            (swoop--read-from-string $query swoop-buffer))
        (when (get-buffer swoop-buffer)
          (swoop--clear-overlay :$kill t)
          (swoop--invisible-off))
        (delete-overlay swoop-target-buffer-overlay)))))

(defcustom swoop-point-at-function (lambda () (thing-at-point 'symbol))
  "Change pre input action. Default is get symbol where cursor at"
  :group 'swoop
  :type 'symbol)
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
      (swoop--core :$resume t :$query swoop--last-query-plain)
    (swoop--core :$query (or $query (swoop--pre-input)))))
;;;###autoload
(defun swoop-pcre-regexp (&optional $query)
  (interactive)
  (let ((swoop-use-pcre t))
    (if current-prefix-arg
        (swoop--core :$resume t :$query swoop--last-query-plain)
      (swoop--core :$query (or $query (swoop--pre-input))))))
;;;###autoload
(defun swoop-migemo (&optional $query)
  (interactive)
  (let ((swoop-use-migemo t))
    (if current-prefix-arg
        (swoop--core :$resume t :$query swoop--last-query-plain)
      (swoop--core :$query (or $query (swoop--pre-input))))))
;;;###autoload
(defun swoop-line-length-over80 (&optional $query) (interactive) (swoop--core :$query "^[^\n]\\{80,\\}"))

;; Match manipulation
(defvar swoop--keep-buffer-position 1)
(defun swoop-update ($query $buf)
  (when (get-buffer $buf)
    ;; Issue a session ID
    (setq swoop--async-id-latest (symbol-name (cl-gensym)))
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
              (swoop--async-divider $query)
              ;; (swoop--move-line 'init)
              ))))))
(defsubst swoop--make-same-element-list ($list1 $list2)
  (let ($result)
    (let (($nth 0))
      (while $list1
        (if (swoop--old-session?) (cl-return))
        (let (($top (car $list1)))
          (when (memq $top $list2)
            (setq $result (cons $top $result))))
        (setq $nth (1+ $nth))
        (setq $list1 (cdr $list1))))
    (nreverse $result)))
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
            (if (swoop--old-session?) (cl-return))
            (setq $results (swoop--make-same-element-list $results $l)))))
    (setq swoop--last-visible-lines $results)))
(cl-defun swoop--async-get-match-lines-list ($query &optional $from)
  (save-excursion
    (goto-char (point-min))
    (let* (($po (re-search-forward $query nil t))
           ($match-lines nil))
      (cl-block stop
        (while $po
          (setq $match-lines
                (cons
                 (if (bolp)
                     ;; include end of return match (e.g. def[^u])
                     (1- (+ (line-number-at-pos) $from))
                   (+ (line-number-at-pos) $from))
                 $match-lines))
          (forward-line 1)
          (if (eq $po (setq $po (re-search-forward $query nil t)))
              (cl-return-from stop nil))))
      $match-lines)))
(defvar swoop--async-fn (byte-compile 'swoop--async-get-match-lines-list))
(defsubst swoop--hash-values-to-list ($hash)
  (let ($results)
    (maphash (lambda (ignored $val)
               (setq $results (cons $val $results))) $hash)
    $results))
(cl-defun swoop--words-overlay ($pattern $line-format)
  (setq swoop--keep-buffer-position (point))
  (swoop--invisible-off)
  (put-text-property (point-min) (point-max) 'invisible 'swoop)
  (swoop--clear-overlay)
  ;; Delete counting key
  (remhash swoop--async-id-latest swoop--async-pool)
  (swoop--mapc $l (swoop--match-lines-list-common
                   (swoop--hash-values-to-list swoop--async-pool))
    (if (swoop--old-session?) (cl-return))
    (swoop--goto-line $l)
    (let* (($lbeg (line-beginning-position))
           ($lend (line-end-position))
           ($lov (make-overlay $lbeg $lend))
           ($pos))
      ;; Show lines
      (put-text-property $lbeg (min (1+ $lend) (point-max)) 'invisible nil)
      ;; Line number overlay
      ;; (overlay-put $lov 'before-string
      ;;              (propertize
      ;;               (format $line-format $l)
      ;;               'face 'swoop-line-number-face))
      ;; (overlay-put $lov 'swoop-temporary t)
      ;; Words overlay
      (cl-block stop
        (setq $pos (re-search-forward $pattern $lend t))
        (while $pos
          (if (swoop--old-session?) (cl-return))
          (let* (($wbeg (match-beginning 0))
                 ($wend (match-end 0))
                 ($ov (make-overlay $wbeg $wend)))
            (overlay-put $ov 'face 'swoop-target-words-face)
            (overlay-put $ov 'swoop-temporary t)
            (overlay-put $ov 'priority 20)
            (with-selected-window swoop--target-window
              (setq $ov (make-overlay $wbeg $wend))
              (overlay-put $ov 'face 'swoop-target-words-face)
              (overlay-put $ov 'swoop-temporary t)
              (overlay-put $ov 'priority 20))
            (if (eq $pos
                    (setq $pos (re-search-forward $pattern $lend t)))
                (cl-return-from stop nil)))))))
  (swoop--invisible-on)
  ;; Adjust position
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
           (swoop--prev-line)))))
(defun swoop--get-point-from-line ($line &optional $buf)
  (or $buf (setq $buf (current-buffer)))
  (save-excursion
    (with-current-buffer $buf
      (swoop--goto-line $line)
      ;; Must subtract 1 for extract buffer contents,
      ;; by substring-no-properties
      (1- (point)))))
(defun swoop--async-checker ($result $length $pattern $line-format)
  (let* (($tag (car $result))
         ($check-key (car $tag)))
    (if (equal swoop--async-id-latest $check-key)
        (let (($key (cdr $tag))
              ($match (cdr $result)))
          (progn
            (let (($v (gethash $check-key swoop--async-pool)))
              (if $v
                  (puthash $check-key (1+ $v) swoop--async-pool)
                (puthash $check-key 1 swoop--async-pool)))
            (let (($v (gethash $key swoop--async-pool)))
              (if $v
                  ;; Add results if the same $key already exists
                  (puthash $key (append $v $match) swoop--async-pool)
                (puthash $key $match swoop--async-pool)))
            (if (eq $length (gethash $check-key swoop--async-pool))
                (swoop--words-overlay $pattern $line-format))))
      (setq swoop--async-id-last swoop--async-id-latest))))
(defsubst swoop--old-session? ()
  (not (eq swoop--async-id-last swoop--async-id-latest)))
(cl-defun swoop--async-divider ($query)
  (setq swoop--async-id-last swoop--async-id-latest)
  (let* (($mhhatch-lines-list nil)
         ($pos-max (point-max))
         ($length (length $query))
         ($max-line (line-number-at-pos $pos-max))
         ($max-line-digit (length (number-to-string $max-line)))
         ($line-format (concat "%0" (number-to-string $max-line-digit) "s: "))
         ($pattern (concat "\\(" (mapconcat 'identity $query "\\|") "\\)"))
         ($lby 500)               ; Buffer divide by
         ($ln (/ $max-line $lby)) ; Result of division
         ($lr (% $max-line $lby)) ; Rest of division
         ;; Number of divided parts of a buffer
         ($bn (if (eq 0 $lr) $ln (1+ $ln)))
         ($tots (* $bn $length))  ; Total session
         ($query-id))
    (save-excursion
      (swoop--mapc $q $query
        (setq $query-id (symbol-name (cl-gensym)))
        (cl-dotimes ($i $bn)
          (if (swoop--old-session?) (cl-return))
          (async-start
           `(lambda ()
              (fundamental-mode)
              (insert ,(substring-no-properties
                        swoop--buffer-content
                        (swoop--get-point-from-line
                         (1+ (* $i $lby))
                         swoop--target-buffer)
                        ;; To show the last line
                        (if (> (* (1+ $i) $lby) $max-line)
                            nil
                          (swoop--get-point-from-line
                           (min $max-line (* (1+ $i) $lby))
                           swoop--target-buffer))))
              (goto-char (point-min))
              (cons (cons ,swoop--async-id-latest ,$query-id)
                    (funcall ,swoop--async-fn ,$q ,(* $i $lby))))
           `(lambda ($result)
              (when (get-buffer ,swoop-buffer)
                (with-current-buffer ,swoop-buffer
                  (swoop--async-checker
                   $result ,$tots ,$pattern ,$line-format)
                  )))
              ))))))

;; Converter
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
(defvar swoop-migemo-options
  "-q -e -d /usr/local/share/migemo/utf-8/migemo-dict")
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

;; Minibuffer
(defvar swoop-input-dilay 0)
(defvar swoop-input-threshold 2)
(defvar swoop--minibuffer-history nil)
(defun swoop--read-from-string ($query $buf)
  (let (($timer nil)
        ($first t))
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
                                  (>= (length $content)
                                      swoop-input-threshold)))
                        ;; Stop old async process
                        (clrhash swoop--async-pool)
                        (setq swoop--minibuf-last-content $content)
                        (swoop-update (swoop--convert-input $content) $buf)
                        )))))))
          (read-from-minibuffer
           "Swoop: " (or $query "")
           swoop-map nil swoop--minibuffer-history nil t))
      (when $timer (cancel-timer $timer) (setq $timer nil))
      (setq swoop--last-query-plain swoop--minibuf-last-content)
      (setq swoop--minibuf-last-content "")
      (recenter))))

;; @ Edit mode ----------------------------------------------------------
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
  (select-window swoop--target-window)
  (kill-buffer swoop-edit-buffer))
(defun swoop--modify-buffer-content ($bufcont)
  "Modify the original buffer content, but it causes slow rendering."
  $bufcont)
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
(defun swoop--edit-insert-lines ($buf $visible-lines)
  (dolist ($l (swoop--get-match-line-content $buf $visible-lines))
    (insert (format "%s%s\n" (car $l) (cdr $l)))))

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
       ;; Header
       (insert (propertize
                (concat " " $bufname "\n")
                'face
                '(:height 1.5 :background "#333333" :foreground "#eeeeee")
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
     (buffer-name swoop--target-buffer)
     swoop--target-window
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
          (put-text-property $beg $end 'swoop-target $m)
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
