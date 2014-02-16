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

(require 'swoop-lib)


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
                  (point-at-bol (- 0 $around))
                  (point-at-bol $around)))
           (overlay-put swoop-magnify-around-target-overlay
                        'face `(:height ,$size))
           (overlay-put swoop-magnify-around-target-overlay
                        'priority 100))
          ((and $delete swoop-magnify-around-target-overlay)
           (delete-overlay swoop-magnify-around-target-overlay))
          (t
           (move-overlay
            swoop-magnify-around-target-overlay
            (point-at-bol (- 0 $around))
            (point-at-bol $around))))))

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
         ($lend (point-at-eol))
         ($lov (make-overlay $lbeg $lend))
         ($lbeg2 (point-at-bol))
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
       (let* (($lbeg (point-at-bol))
              ($lend (point-at-eol))
              ($lov (make-overlay $lbeg $lend))
              ($wbeg (match-beginning 0))
              ($wend (match-end 0))
              ($wov (make-overlay $wbeg $wend)))
         (run-with-timer 0.28 nil (lambda ($o) (delete-overlay $o)) $lov)
         (overlay-put $lov 'face 'swoop-target-words-face)
         (run-with-timer 0.4 nil (lambda ($o) (delete-overlay $o)) $wov)
         (overlay-put $wov 'face 'swoop-target-line-face)
         (recenter))))
   (with-selected-window swoop--target-window (point)))
  (exit-minibuffer))

;; Move line up and down

(defsubst swoop--move-line-within-target-window ($line-num $buf)
  (with-selected-window swoop--target-window
    (with-current-buffer $buf
      (set-window-buffer nil $buf)
      (swoop--goto-line $line-num)
      (recenter)
      (move-overlay
       swoop-target-buffer-selection-overlay
       (point) (min (1+ (point-at-eol)) (point-max))
       (get-buffer $buf)))
    (if swoop-use-target-magnifier:
        (swoop--magnify-around-target))
    (swoop--unveil-invisible-overlay)
    (setq swoop-last-selected-buffer $buf)))

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
            (goto-char (point-at-bol)))))))
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
            (goto-char (point-at-bol)))))))
(cl-defun swoop--move-line ($direction)
  (with-selected-window swoop-window
    (let ($line-num)
      (cl-case $direction
        (up   (swoop--backward-visible-line))
        (down (swoop--forward-visible-line))
        (init (cond
               ((and (bobp) (eobp))
                (cl-return-from swoop--move-line nil))
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
       (point) (min (1+ (point-at-eol)) (point-max)))
      (swoop--move-line-within-target-window
       (get-text-property (point-at-bol) 'swoop-line)
       (get-text-property (point-at-bol) 'swoop-buf))
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
    (if swoop-target-buffer-selection-overlay
        (delete-overlay swoop-target-buffer-selection-overlay))
    (unless $kill
      (if swoop-buffer-selection-overlay
          (delete-overlay swoop-buffer-selection-overlay))))
  (if (and $kill (get-buffer swoop-buffer))
      (kill-buffer swoop-buffer)))
(defsubst swoop--buffer-selection-overlay-set ()
  (setq swoop-buffer-selection-overlay
        (make-overlay (point-at-bol)
                      (min (1+ (point-at-eol)) (point-max))))
  (overlay-put swoop-buffer-selection-overlay 'face 'swoop-target-line-face)
  (overlay-put swoop-buffer-selection-overlay 'priority 15))
(defsubst swoop--target-buffer-selection-overlay-set ()
  (setq swoop-target-buffer-selection-overlay
        (make-overlay (point-at-bol)
                      (min (1+ (point-at-eol)) (point-max))))
  (overlay-put swoop-target-buffer-selection-overlay
               'face 'swoop-target-line-face)
  (overlay-put swoop-target-buffer-selection-overlay 'priority 15))

(defvar swoop--async-pool (make-hash-table :test 'equal))
(defvar swoop--async-id-latest nil)
(cl-defun swoop--core (&key $query $resume $multi)
  (setq
   swoop--last-position (point)
   swoop--target-buffer (current-buffer)
   swoop--target-window (get-buffer-window swoop--target-buffer))
  (setq $multi t) ;; ff1
  (if $multi
      (swoop--set-buffer-info-all)
    (swoop--set-buffer-info swoop--target-buffer))
  ;; Font size change
  ;; (setq swoop-target-buffer-overlay (make-overlay (point-min) (point-max)))
  ;; (overlay-put swoop-target-buffer-overlay 'face `(:height ,swoop-font-size-change:))
  ;; (recenter)
  (swoop--target-buffer-selection-overlay-set)
  (save-window-excursion
    (progn
      (funcall swoop-display-function swoop-buffer)
      ;; (swoop--invisible-on)
      (erase-buffer)
      (setq swoop-window (get-buffer-window swoop-buffer))
      (make-local-variable 'swoop--target-buffer)
      (make-local-variable 'swoop--target-window)
      (make-local-variable 'swoop--target-buffer-info)
      (make-local-variable 'swoop--last-position)
      ;; (insert (swoop--modify-buffer-content
      ;;          (ht-get swoop--target-buffer-info "buf-content")))
      ;; (goto-char swoop--last-position)
      (swoop--buffer-selection-overlay-set))
    (unwind-protect
        (when (get-buffer swoop-buffer)
          (ht-clear! swoop--async-pool)
          (when (or $query $resume)
            ;; Prevent following minibuffer session once
            (setq swoop--minibuf-last-content $query)
            ;; First time
            (if (or (listp $query) $resume)
                (swoop-update swoop--last-query-converted)
              (swoop-update (split-string $query " " t))))
          (swoop--read-from-string $query swoop-buffer))
      (when (get-buffer swoop-buffer)
        (swoop--clear-overlay :$kill t)
        (swoop--invisible-off))
      (delete-overlay swoop-target-buffer-overlay))))

(defcustom swoop-point-at-function
  (lambda () (thing-at-point 'symbol))
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
(defun swoop-update ($query)
  (when (get-buffer swoop-buffer)
    ;; Issue a session ID
    (setq swoop--async-id-latest (symbol-name (cl-gensym)))
    (unless (listp $query)
      (setq $query (split-string $query " " t)))
    (setq swoop--last-query-converted $query)
    (with-current-buffer swoop-buffer
      (if (not $query)
          (swoop--clear-overlay :$to-empty t)
        (swoop--async-divider $query)))))
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
(defsubst swoop--hash-values-to-list ($hash)
  (let ($results)
    (maphash (lambda (ignored $val)
               (setq $results (cons $val $results))) $hash)
    $results))
(defun swoop--get-point-from-line ($line &optional $buf)
  (or $buf (setq $buf (current-buffer)))
  (save-excursion
    (with-current-buffer $buf
      (swoop--goto-line $line)
      ;; Must subtract 1 for extract buffer contents,
      ;; by substring-no-properties
      (1- (point)))))

(setq swoop-buffer-info (ht-create 'equal))
;; (setq swoop-buffer-info nil)
;; (ht-size swoop-buffer-info)
;; (swoop--set-buffer-info (current-buffer))
(defun swoop--set-buffer-info ($buf)
  (with-current-buffer $buf
    (let* (($buf-content (buffer-substring-no-properties (point-min) (point-max)))
           ($point-min (point-min))
           ($point-max (point-max))
           ($max-line (line-number-at-pos $point-max))
           ($max-line-digit (length (number-to-string $max-line)))
           ($line-format (concat "%0" (number-to-string $max-line-digit) "s: "))
           ($by 3000)               ; Buffer divide by
           ($res (/ $max-line $by)) ; Result of division
           ($rest (% $max-line $by)) ; Rest of division
           ;; Number of divided parts of a buffer
           ($buf-num (if (eq 0 $rest) $res (1+ $res)))
           ($separated-buffer))
      (let (($with-end-break (concat $buf-content "\n")))
        (cl-dotimes ($i $buf-num)
          (setq $separated-buffer
                (cons
                 (substring-no-properties
                  $with-end-break
                  (swoop--get-point-from-line (1+ (* $i $by)) $buf)
                  ;; To show the last line
                  (if (>= (* (1+ $i) $by) $max-line)
                      nil
                    (swoop--get-point-from-line
                     (min $max-line (1+ (* (1+ $i) $by))) $buf)))
                 $separated-buffer))))
      (setq swoop--target-buffer-info
            (ht ("buf-name" $buf)
                ("buf-content" $buf-content)
                ("buf-separated" (nreverse $separated-buffer))
                ("buf-number" $buf-num)
                ("point-min" $point-min)
                ("point-max" $point-max)
                ("max-line" $max-line)
                ("max-line-digit" $max-line-digit)
                ("line-format" $line-format)
                ("divide-by" $by)))
      (ht-set swoop-buffer-info $buf swoop--target-buffer-info)))
  nil)
;; ----------------------------------------------------------------------
;; Async
(setq swoop--async-id-last nil)
(defsubst swoop--old-session? ()
  (not (equal swoop--async-id-last swoop--async-id-latest)))
(defun swoop--async-get-match-lines-list
    ($query $from $line-format $line-face $buf)
  (save-excursion
    (let* (($lines nil)
           ($pos-min (point-min))
           ($pos-max (point-max))
           (buffer-invisibility-spec nil)
           ($match-lines)
           ($match-total)
           ($po t))
      (goto-char $pos-min)
      (put-text-property (point-min) (point-max) 'swoop-buf $buf)
       ;; Get lines at least one match
      (mapc (lambda ($q)
              (save-excursion
                (goto-char $pos-min)
                (while (re-search-forward $q nil t)
                  (setq $match-lines (cons (line-number-at-pos) $match-lines))
                  (forward-line))
                (setq $match-total (cons $match-lines $match-total))
                (setq $match-lines nil)))
            $query)
      ;; Culling all words match lines
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
                          'swoop-line $line-num)
                         $lines))))
              $results))
      ;; (nreverse $lines)
      (or $lines '("")))))
(setq swoop--async-get-match-lines-list
      (byte-compile 'swoop--async-get-match-lines-list))
(defun swoop--async-checker ($result $tots $pattern)
  (let* (($id (car $result))
         ($check-key (car $id)))
    (if (equal swoop--async-id-latest $check-key)
        (let (($buf (cdr $id))
              ($lines (cdr $result))
              ($total))
          (progn
            (let (($v (ht-get swoop--async-pool $buf)))
              (if $v
                  (progn
                    (ht-set swoop--async-pool
                            $buf
                            (cons $lines $v)))
                (ht-set swoop--async-pool
                        $buf
                        (cons $lines nil))))
            (let (($n (or (ht-get swoop--async-pool "number") 0)))
              (ht-set swoop--async-pool "number" (1+ $n))
              (if (eq $tots (1+ $n))
                  (swoop--words-overlay $pattern)))))
      (setq swoop--async-id-last swoop--async-id-latest))))
;;(echo swoop--async-pool)
(cl-defun swoop--words-overlay ($pattern)
  (swoop--clear-overlay)
  (with-selected-window swoop-window
    (setq swoop--keep-buffer-position (point))
    (let (($cont ""))

      (erase-buffer)

      (ht-remove swoop--async-pool "number")
      (ht-each (lambda ($buf $val)
                 (let* (($con "")
                        ($length (length $val)))
                   (if (eq 1 $length)
                       (setq $con (mapconcat 'identity (cdar $val) ""))
                     (swoop--mapc $p (cl-sort $val 'string< :key 'car)
                       (unless (equal $p "")
                         (setq $con (concat $con (mapconcat 'identity (cdr $p) ""))))))
                   (when (not (equal "" $con))
                     (setq $con
                           (concat
                            (propertize
                             (concat $buf "\n")
                             'face
                             '(:height 1.5 :background "#ff9900" :foreground "#222222")
                             'swoop-header t)
                            $con)))
                   (setq $cont (concat $cont $con)))
                 )
               swoop--async-pool)
      (insert $cont)
      (goto-char (point-min))

      )

      ;; (swoop--mapc $parts $new-buffer
      ;;   (setq $cont (concat $cont (mapconcat 'identity (cdr $parts) ""))))
      ;; (insert $cont))

    (save-excursion
      (let (($po (point-min)))
        (goto-char $po)
        (put-text-property
         $po (setq $po (next-single-property-change $po 'swoop-header))
         'intangible t)
      (while (setq $po (next-single-property-change $po 'swoop-header))
        (put-text-property
         (1- $po) (setq $po (next-single-property-change $po 'swoop-header))
         'intangible t))))

    ;; Words overlay
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward $pattern nil t)
        (if (swoop--old-session?) (cl-return-from stop1))
        (let* (($beg (match-beginning 0))
               ($end (match-end 0))
               ($ov (make-overlay $beg $end))
               ($ov2))
          (if (eq $beg $end) (cl-return))
          (overlay-put $ov 'face 'swoop-target-words-face)
          (overlay-put $ov 'swoop-temporary t)
          (overlay-put $ov 'priority 20)
          ;; (with-selected-window swoop--target-window
          ;;   (setq $ov2 (make-overlay $beg $end))
          ;;   (overlay-put $ov2 'face 'swoop-target-words-face)
          ;;   (overlay-put $ov2 'swoop-temporary t)
          ;;   (overlay-put $ov2 'priority 20))
          )))
    ;; Words overlay
    (with-selected-window swoop--target-window
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward $pattern nil t)
          (if (swoop--old-session?) (cl-return-from stop2))
          (let* (($beg (match-beginning 0))
                 ($end (match-end 0))
                 ($ov (make-overlay $beg $end))
                 ($ov2))
            (if (eq $beg $end) (cl-return))
            (overlay-put $ov 'face 'swoop-target-words-face)
            (overlay-put $ov 'swoop-temporary t)
            (overlay-put $ov 'priority 20)))))

    ;; Line number overlay
    ;; (overlay-put $lov 'before-string
    ;;              (propertize
    ;;               (format $line-format $l)
    ;;               'face 'swoop-line-number-face))
    ;; (overlay-put $lov 'swoop-temporary t)

    ;; Adjust position
    ;; (with-selected-window swoop-window
    ;;   ;; ;; Keep position near where the cursor was at before update the list
    ;;   ;; (goto-char swoop--keep-buffer-position)
    ;;   ;; (swoop--move-line 'init)
    ;;   ;; ;; ;; Back to inner list if the cursor at the top or at the bottom of the list
    ;;   (cond ((and (bobp) (eobp))
    ;;          nil)
    ;;         ((bobp)
    ;;          (swoop--next-line))
    ;;         ((eobp)
    ;;          (swoop--prev-line)))
    ;;   )
    ))
(cl-defun swoop--async-divider ($query)
  (with-current-buffer swoop-buffer
    (setq swoop--async-id-last swoop--async-id-latest)
    (let* (($pattern (concat "\\(" (mapconcat 'identity $query "\\|") "\\)"))
           ($tots (let (($r 0))
                    (swoop--mapc $n (swoop--buffer-info-get-map "buf-number")
                      (setq $r (+ $n $r)))
                    $r))
           ;; ($separated-buffer
           ;;  (ht-get swoop--target-buffer-info "buf-separated"))
           ;; ($by (ht-get swoop--target-buffer-info "divide-by"))
           )

      (ht-each
       (lambda ($buf $buf-hash)
         (if (swoop--old-session?) (cl-return-from swoop--async-divider))
         (let* (($tot     (ht-get $buf-hash "buf-number"))
                ($buf-sep (ht-get $buf-hash "buf-separated"))
                ($by      (ht-get $buf-hash "divide-by"))
                ($buf-sep-id))
           (cl-dotimes ($i $tot)
             (setq $buf-sep-id (symbol-name (cl-gensym)))
             (if (swoop--old-session?) (cl-return-from swoop--async-divider))
             (async-start
              `(lambda ()
                 (fundamental-mode)
                 (insert ,(nth $i $buf-sep))
                 (cons (cons ,swoop--async-id-latest ,$buf)
                       (cons ,$buf-sep-id
                             (funcall ,swoop--async-get-match-lines-list
                                      ',$query ,(* $i $by)
                                      ,(ht-get swoop--target-buffer-info
                                               "line-format")
                                      ',swoop-line-number-face
                                      ,$buf))))
              (lambda ($result)
                (when (get-buffer swoop-buffer)
                  (with-current-buffer swoop-buffer
                    (swoop--async-checker $result $tots $pattern)))
                )))))
       swoop-buffer-info)

      )))

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
    (setq $input (swoop--migemo-convert $input)))
   (t $input))
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
                    (let* (($content (format "%s" (minibuffer-contents))))
                      (when (and (not (equal swoop--minibuf-last-content
                                             $content))
                                 (or
                                  ;; When becomeing empty again
                                  (equal "" $content)
                                  ;; Avoid too many matching
                                  (>= (length $content)
                                      swoop-input-threshold)))
                        ;; Stop old async process
                        (ht-clear! swoop--async-pool)
                        (setq swoop--minibuf-last-content $content)
                        (swoop-update (swoop--convert-input $content))
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
        ;; (put-text-property (point-at-bol) $eol
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
                         (point-at-bol) (point-at-eol)))
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
    (let* (($line-beg (point-at-bol))
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
               ($line-end (point-at-eol))
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
                (overlays-in (point-at-bol)
                             (point-at-eol))
              (let (($type (overlay-get $ov 'invisible)))
                (when $type
                  (overlay-put $ov 'invisible nil))))
            (recenter $line)
            (if $readonly
                (message "Buffer `%s' is read only." $buf)
              (delete-region (point-at-bol) (point-at-eol))
              (insert $text))
            (move-to-column $col)))))))
;; ----------------------------------------------------------------------

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
(defun swoop--set-buffer-info-all ()
  (let (($bufs (swoop-multi-get-buffer-list)))
    (swoop--mapc $buf $bufs
      (unless (and (member $buf (ht-keys swoop-buffer-info))
                   (not (with-current-buffer $buf (buffer-modified-p))))
        (swoop--set-buffer-info $buf)))
    (swoop--mapc $buf (ht-keys swoop-buffer-info)
      (unless (member $buf $bufs)
        (ht-remove! swoop-buffer-info $buf)))))
(defun swoop--buffer-info-get ($buf $key2)
  (ht-get (ht-get swoop-buffer-info $buf) $key2))
(defun swoop--buffer-info-get-map ($key)
  (ht-map (lambda ($bname $binfo)
            (ht-get $binfo $key))
          swoop-buffer-info))



(provide 'swoop)
;;; swoop.el ends here
