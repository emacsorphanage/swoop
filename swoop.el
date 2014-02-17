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
(require 'swoop-async)
(provide 'swoop-edit)

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
          (t (move-overlay
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
  "This is assigned to `C-g' as default. Exit from Swoop (minibuffer)
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
   (lambda ($info)
     (with-selected-window swoop--target-window
       (with-current-buffer (cdr $info)
         (set-window-buffer nil (cdr $info))
         (goto-char (car $info))
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
           (recenter)))))
   (with-selected-window swoop--target-window (cons (point) (current-buffer))))
  (exit-minibuffer))


;; Move line up and down
(defsubst swoop--move-line-within-target-window ($line-num $buf)
  (with-selected-window swoop--target-window
    (when (not (equal $buf swoop-last-selected-buffer))
      (with-current-buffer $buf
        (set-window-buffer nil $buf))
      (with-selected-window swoop-window
        (setq header-line-format
              (propertize $buf 'face '(:height 1.4 :foreground "#ff9900")))))
    (swoop--goto-line $line-num)
    (recenter)
    (move-overlay
     swoop-target-buffer-selection-overlay
     (point) (min (1+ (point-at-eol)) (point-max))
     (get-buffer $buf))
      (if swoop-use-target-magnifier:
        (swoop--magnify-around-target))
    (swoop--unveil-invisible-overlay)
    (setq swoop-last-selected-buffer $buf)))

(defsubst swoop--forward-line ()
  (let ($po)
    (if (get-text-property
         (setq $po (next-single-property-change (point) 'swl))
         'swl)
        (goto-char $po)
      (if (get-text-property
           (setq $po (next-single-property-change $po 'swl))
           'swl)
          (goto-char $po)))))
(defsubst swoop--backward-line ()
  (let ($po)
    (if (get-text-property
         (setq $po (previous-single-property-change (point) 'swl))
         'swl)
        (goto-char $po)
      (if (get-text-property
           (setq $po (previous-single-property-change $po 'swl))
           'swl)
          (goto-char $po)))))
(cl-defun swoop--move-line ($direction)
  (with-selected-window swoop-window
    (let ($line-num)
      (cl-case $direction
        (up   (swoop--backward-line))
        (down (swoop--forward-line))
        (init (cond
               ((and (bobp) (eobp))
                (cl-return-from swoop--move-line nil))
               ((bobp)
                (swoop--forward-line)
                (move-beginning-of-line 1))
               ((eobp)
                (swoop--backward-line)
                (move-beginning-of-line 1))
               (t (move-beginning-of-line 1))
               )))
      (move-overlay
       swoop-buffer-selection-overlay
       (point) (min (1+ (point-at-eol)) (point-max)))
      (swoop--move-line-within-target-window
       (get-text-property (point-at-bol) 'swl)
       (get-text-property (point-at-bol) 'swb))
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
  (swoop--mapc $buf (ht-keys swoop-buffer-info)
    (if (swoop--old-session?) (cl-return-from swoop--clear-overlay))
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
(cl-defun swoop--word-overlay ($pattern $buf)
  (with-current-buffer $buf
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward $pattern nil t)
        (if (swoop--old-session?) (cl-return-from stop1))
        (let* (($beg (match-beginning 0))
               ($end (match-end 0))
               ($ov (make-overlay $beg $end)))
          (if (eq $beg $end) (cl-return-from swoop--word-overlay))
          (overlay-put $ov 'face 'swoop-target-words-face)
          (overlay-put $ov 'swoop-temporary t)
          (overlay-put $ov 'priority 20))))))


(cl-defun swoop--core (&key $query $resume $multi)
  (setq
   swoop--last-position (point)
   swoop--last-line (line-number-at-pos)
   swoop--target-buffer (buffer-name (current-buffer))
   swoop--target-window (get-buffer-window swoop--target-buffer))
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
      (setq swoop-window (get-buffer-window swoop-buffer))
      (make-local-variable 'swoop--target-buffer)
      (make-local-variable 'swoop--target-window)
      (make-local-variable 'swoop--target-buffer-info)
      (make-local-variable 'swoop--last-position)
      (make-local-variable 'swoop--last-line)
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
                (swoop-update swoop--last-query-converted $multi)
              (swoop-update (split-string $query " " t) $multi)))
          (swoop--read-from-string $query swoop-buffer $multi))
      (when (get-buffer swoop-buffer)
        (swoop--clear-overlay :$kill t)
        (swoop--invisible-off))
      ;; (swoop--kill-process-buffer)
      (if swoop-target-buffer-overlay
          (delete-overlay swoop-target-buffer-overlay)))))

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
(defun swoop-multi (&optional $query)
  (interactive)
  (if current-prefix-arg
      (swoop--core :$resume t :$query swoop--last-query-plain :$multi t)
    (swoop--core :$query (or $query (swoop--pre-input)) :$multi t)))
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

(defun swoop-multi-from-swoop ()
  (interactive)
  (run-with-timer
   0 nil
   (lambda ()
     (swoop--core :$resume t :$query swoop--last-query-plain :$multi t)))
  (exit-minibuffer))
(define-key swoop-map (kbd "C-o") 'swoop-multi-from-swoop)


;; Match manipulation
(defvar swoop--keep-buffer-position 1)
(defun swoop-update ($query $multi)
  (when (get-buffer swoop-buffer)
    (swoop--kill-process)
    ;; Issue a session ID
    (setq swoop--async-id-latest (symbol-name (cl-gensym)))
    (unless (listp $query)
      (setq $query (split-string $query " " t)))
    (setq swoop--last-query-converted $query)
    (with-current-buffer swoop-buffer
      (if (not $query)
          (swoop--clear-overlay :$to-empty t)
        (swoop--async-divider $query $multi)))))
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
;; (defun swoop--match-lines-list-common ($match-lines-list)
;;   "Return common numbers list of several numbers lists.
;;  (swoop--match-lines-list-common '((1 2 3 4 5) (2 3 4 8) (2 3 9))) -> '(2 3)"
;;   (let* (($list $match-lines-list)
;;          ($results)
;;          ($length (length $list)))
;;     (when (> $length 0)
;;       (setq $results (car-safe $list))
;;       (if (> $length 1)
;;           (swoop--mapc $l (cdr $list)
;;             (if (swoop--old-session?) (cl-return))
;;             (setq $results (swoop--make-same-element-list $results $l)))))
;;     (setq swoop--last-visible-lines $results)))




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
              (when (eq $tots (1+ $n))
                (swoop--render $pattern)))))
      (setq swoop--async-id-last swoop--async-id-latest))))

(cl-defun swoop--render ($pattern)
  (swoop--clear-overlay)
  (setq swoop-last-selected-buffer
        (or (get-text-property (point-at-bol) 'swb)
            swoop--target-buffer))
  (setq swoop-last-selected-line
        (or (get-text-property (point-at-bol) 'swl)
            swoop--last-line
            (ht-get (ht-get swoop-buffer-info swoop--target-buffer) "max-line")
            1))
  (erase-buffer)
  (with-selected-window swoop-window
    (let (($cont "")
          ($match-lines-common)
          ($nearest-line))
      (erase-buffer)
      (ht-remove swoop--async-pool "number")
      ;; swoop--async-pool ;=> (($parts-id $match-list $content-list) ...)
      (ht-each (lambda ($buf $val)
                 (if (swoop--old-session?) (cl-return-from swoop--render))
                 (let (($con "")
                       ($length (length $val)))
                   (if (eq 1 $length)
                       (progn
                         (setq $con (mapconcat 'identity (cddar $val) ""))
                         (if (equal $buf swoop-last-selected-buffer)
                             (setq $match-lines-common (nth 1 (car $val)))))
                     (swoop--mapc $p (cl-sort $val 'string< :key 'car)
                       (unless (equal $p "")
                         (setq $con (concat $con (mapconcat 'identity (cddr $p) "")))
                         (if (equal $buf swoop-last-selected-buffer)
                             (setq $match-lines-common
                                   (append (nth 1 (car $val)) $match-lines-common))))))
                   (when (not (equal "" $con))
                     (setq $con
                           (concat
                            (propertize
                             (concat $buf "\n")
                             'face
                             `(:height 1.3 :background "#0099cc" :foreground "#222222")
                             'swoop-header t
                             'swd (if (equal $buf swoop-last-selected-buffer) t nil))
                            $con)))
                   (setq $cont (concat $cont $con))))
               swoop--async-pool)
      (insert $cont)
      (setq $nearest-line
            (swoop--nearest-line swoop-last-selected-line $match-lines-common))
      ;; Words overlay
      (swoop--word-overlay $pattern swoop-buffer)
      (swoop--mapc $b (ht-keys swoop-buffer-info)
        (if (swoop--old-session?) (cl-return-from swoop--render))
        (swoop--word-overlay $pattern $b))
      ;; Adjust position
      (with-selected-window swoop-window
        (goto-char (or (next-single-property-change (point-min) 'swl) (point-min)))
        (if $nearest-line
            (goto-char
             (or (text-property-any
                  (text-property-any (point-min) (point-max) 'swd t)
                  (point-max) 'swl $nearest-line)
                 (point-min))))
        (cond ((and (bobp) (eobp))
               nil)
              ((bobp)
               (swoop--next-line))
              ((eobp)
               (swoop--prev-line)))
        (swoop--move-line 'init)))))

(cl-defun swoop--async-divider ($query &optional $multi)
  (with-current-buffer swoop-buffer
    (setq swoop--async-id-last swoop--async-id-latest)
    (let (($pattern (concat "\\(" (mapconcat 'identity $query "\\|") "\\)"))
          ($tots (let (($r 0))
                   (swoop--mapc $n (swoop--buffer-info-get-map "buf-number")
                     (setq $r (+ $n $r)))
                   $r)))
      (unless $multi
        (let* (($buf         swoop--target-buffer)
               ($buf-hash    (ht-get swoop-buffer-info $buf))
               ($tot         (ht-get $buf-hash "buf-number"))
               ($buf-sep     (ht-get $buf-hash "buf-separated"))
               ($by          (ht-get $buf-hash "divide-by"))
               ($line-format (ht-get $buf-hash "line-format"))
               ($buf-sep-id))
          (cl-dotimes ($i $tot)
            (setq $buf-sep-id (symbol-name (cl-gensym)))
            (if (swoop--old-session?) (cl-return-from swoop--async-divider))
            (swoop--async-start
             `(lambda ()
                (fundamental-mode)
                (insert ,(nth $i $buf-sep))
                (cons (cons ,swoop--async-id-latest ,$buf)
                      (cons ,$buf-sep-id
                            (funcall ,swoop--async-get-match-lines-list
                                     ',$query ,(* $i $by)
                                     ,$line-format
                                     ',swoop-line-number-face
                                     ,$buf))))
             (lambda ($result)
               (when (get-buffer swoop-buffer)
                 (with-current-buffer swoop-buffer
                   (swoop--async-checker $result $tot $pattern)))
               )))))
      (when $multi
        (ht-each
         (lambda ($buf $buf-hash)
           (if (swoop--old-session?) (cl-return-from swoop--async-divider))
           (let* (($tot         (ht-get $buf-hash "buf-number"))
                  ($buf-sep     (ht-get $buf-hash "buf-separated"))
                  ($by          (ht-get $buf-hash "divide-by"))
                  ($line-format (ht-get $buf-hash "line-format"))
                  ($buf-sep-id))
             (cl-dotimes ($i $tot)
               (setq $buf-sep-id (symbol-name (cl-gensym)))
               (if (swoop--old-session?) (cl-return-from swoop--async-divider))
               (swoop--async-start
                `(lambda ()
                   (fundamental-mode)
                   (insert ,(nth $i $buf-sep))
                   (cons (cons ,swoop--async-id-latest ,$buf)
                         (cons ,$buf-sep-id
                               (funcall ,swoop--async-get-match-lines-list
                                        ',$query ,(* $i $by)
                                        ,$line-format
                                        ',swoop-line-number-face
                                        ,$buf))))
                (lambda ($result)
                  (when (get-buffer swoop-buffer)
                    (with-current-buffer swoop-buffer
                      (swoop--async-checker $result $tots $pattern)))
                  )))))
         swoop-buffer-info)))))


;; Minibuffer
(defun swoop--read-from-string ($query $buf $multi)
  (let (($timer nil)
        ($first t))
    (unwind-protect
        (minibuffer-with-setup-hook
            (lambda ()
              (setq
               $timer
               (run-with-idle-timer
                swoop-minibuffer-input-dilay
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
                        (swoop-update (swoop--convert-input $content) $multi)
                        )))))))
          (read-from-minibuffer
           "Swoop: " (or $query "")
           swoop-map nil swoop-minibuffer-history nil t))
      (when $timer (cancel-timer $timer) (setq $timer nil))
      (setq swoop--last-query-plain swoop--minibuf-last-content)
      (setq swoop--minibuf-last-content "")
      (recenter))))

;; @ Edit mode ----------------------------------------------------------



(provide 'swoop)
;;; swoop.el ends here
