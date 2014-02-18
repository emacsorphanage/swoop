;;; swoop.el --- Peculiar buffer navigation for Emacs -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2014 by Shingo Fukuyama

;; Version: Developing
;; Author: Shingo Fukuyama - http://fukuyama.co
;; URL: https://github.com/ShingoFukuyama/swoop
;; Created: Feb 14 2014
;; Keywords: swoop inner buffer search navigation
;; Package-Requires: ((ht "2.0") (pcre2el "1.5") (async "1.1") (emacs "24"))

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
;; ;; Require
;; ;; async.el   https://github.com/jwiegley/emacs-async
;; ;; pcre2el.el https://github.com/joddie/pcre2el
;; ;; ht.el      https://github.com/Wilfred/ht.el
;; (require 'swoop)
;; (global-set-key (kbd "C-o")   'swoop)
;; (global-set-key (kbd "C-M-o") 'swoop-multi)
;; (global-set-key (kbd "M-o")   'swoop-pcre-regexp)
;; (global-set-key (kbd "C-S-o") 'swoop-back-to-last-position)
;; (global-set-key (kbd "H-6")   'swoop-migemo)

;; ;; Resume
;; ;; C-u M-x swoop : Use last used query

;; ;; Transition
;; ;; During swoop, press [C-o] to target multiple buffers

;; ;; Swoop Edit Mode
;; ;; During swoop, press [C-c C-e]
;; ;; You can edit synchronously

;;; TODO
;; Unpropertize (thing-at-point 'symbol)
;; Prevent long time loop words (], \\b, {0,} ...)

;;; Code:

(require 'swoop-lib)
(require 'swoop-async)
(require 'swoop-edit)

;; Cancel action
(defvar swoop-abort-hook nil)
(defun swoop-action-cancel ()
  "This is assigned to `C-g' as default. Exit from Swoop (minibuffer)
and execute functions listed in swoop-abort-hook"
  (interactive)
  (run-with-timer
   0 nil (lambda () (run-hooks 'swoop-abort-hook)))
  (exit-minibuffer))
(defun swoop-back-to-last-position ()
  (interactive)
  (let (($po swoop--target-last-position))
    (setq swoop--target-last-position (point))
    (with-selected-window swoop--target-window
      (goto-char $po))))
(defun swoop-highlight-for-cancel ()
  (interactive)
  (let* (($lbeg (point))
         ($lend (point-at-eol))
         ($lov (make-overlay $lbeg $lend))
         ($lbeg2 (point-at-bol))
         ($lend2 $lbeg)
         ($lov2 (make-overlay $lbeg2 $lend2)))
    (recenter)
    (run-with-timer 0.3 nil (lambda ($o) (delete-overlay $o)) $lov)
    (overlay-put $lov 'face 'swoop-face-target-words)
    (run-with-timer 0.3 nil (lambda ($o) (delete-overlay $o)) $lov2)
    (overlay-put $lov2 'face 'swoop-face-target-line)))
(add-hook 'swoop-abort-hook 'swoop-back-to-last-position)
(add-hook 'swoop-abort-hook 'swoop-highlight-for-cancel)

;; Default action
(defun swoop-action-goto-target-point ()
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
             (mapconcat 'identity swoop-last-query-converted "\\|")
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
           (overlay-put $lov 'face 'swoop-face-target-words)
           (run-with-timer 0.4 nil (lambda ($o) (delete-overlay $o)) $wov)
           (overlay-put $wov 'face 'swoop-face-target-line)
           (recenter)))))
   (with-selected-window swoop--target-window (cons (point) (current-buffer))))
  (exit-minibuffer))

;; Overlay
(cl-defun swoop-overlay-clear (&key $to-empty $kill $multi)
  (swoop-mapc $buf (if $multi
                      (ht-keys swoop-buffer-info)
                      (list swoop--target-buffer))
    (if (swoop-async-old-session?) (cl-return-from swoop-overlay-clear))
    (when (get-buffer swoop-buffer)
      (with-current-buffer $buf
        (swoop-mapc $ov (overlays-in (point-min) (point-max))
          (if (swoop-async-old-session?) (cl-return))
          (when (overlay-get $ov 'swoop-temporary)
            (delete-overlay $ov))))))
  (if swoop-use-target-magnifier:
      (swoop-magnify-around-target :$delete t))
  (unless $to-empty
    (if swoop-overlay-target-buffer-selection
        (delete-overlay swoop-overlay-target-buffer-selection))
    (unless $kill
      (if swoop-overlay-buffer-selection
          (delete-overlay swoop-overlay-buffer-selection))))
  (if (and $kill (get-buffer swoop-buffer))
      (kill-buffer swoop-buffer)))
(defsubst swoop-overlay-selection-buffer-set ()
  (setq swoop-overlay-buffer-selection
        (make-overlay (point-at-bol)
                      (min (1+ (point-at-eol)) (point-max))))
  (overlay-put swoop-overlay-buffer-selection 'face 'swoop-face-target-line)
  (overlay-put swoop-overlay-buffer-selection 'priority 15))
(defsubst swoop-overlay-selection-target-buffer-set ()
  (setq swoop-overlay-target-buffer-selection
        (make-overlay (point-at-bol)
                      (min (1+ (point-at-eol)) (point-max))))
  (overlay-put swoop-overlay-target-buffer-selection
               'face 'swoop-face-target-line)
  (overlay-put swoop-overlay-target-buffer-selection 'priority 15))
(cl-defun swoop-overlay-word ($pattern $buf)
  (with-current-buffer $buf
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward $pattern nil t)
        (if (swoop-async-old-session?) (cl-return-from stop1))
        (let* (($beg (match-beginning 0))
               ($end (match-end 0))
               ($ov (make-overlay $beg $end)))
          (if (eq $beg $end) (cl-return-from swoop-overlay-word))
          (overlay-put $ov 'face 'swoop-face-target-words)
          (overlay-put $ov 'swoop-temporary t)
          (overlay-put $ov 'priority 20))))))


(cl-defun swoop-core (&key $query $resume $multi)
  (setq
   swoop--target-last-position (point)
   swoop--target-last-line (line-number-at-pos)
   swoop--target-buffer (buffer-name (current-buffer))
   swoop--target-window (get-buffer-window swoop--target-buffer))
  (if $multi
      (swoop-set-buffer-info-all)
    (swoop-set-buffer-info swoop--target-buffer))

  (swoop-overlay-selection-target-buffer-set)
  (save-window-excursion
    (progn
      (funcall swoop-display-function swoop-buffer)
      (setq swoop-window (get-buffer-window swoop-buffer))
      (make-local-variable 'swoop--target-buffer)
      (make-local-variable 'swoop--target-window)
      (make-local-variable 'swoop--target-buffer-info)
      (make-local-variable 'swoop--target-last-position)
      (make-local-variable 'swoop--target-last-line)
      (swoop-overlay-selection-buffer-set))
    (unwind-protect
        (when (get-buffer swoop-buffer)
          (ht-clear! swoop-async-pool)
          (when (or $query $resume)
            ;; Prevent following minibuffer session once
            (setq swoop-minibuf-last-content $query)
            ;; First time
            (if (or (listp $query) $resume)
                (swoop-update swoop-last-query-converted $multi)
              (swoop-update (split-string $query " " t) $multi)))
          (swoop-minibuffer-read-from-string $query $multi))
      (when (get-buffer swoop-buffer)
        (swoop-overlay-clear :$kill t :$multi $multi))
      (if swoop-overlay-target-buffer
          (delete-overlay swoop-overlay-target-buffer))
      ;; Restore last position of other buffers
      (when $multi
        (swoop-mapc $buf (ht-keys swoop-buffer-info)
          (unless (equal $buf (buffer-name (current-buffer)))
            (goto-char (swoop-buffer-info-get $buf "point"))))))))

(defcustom swoop-pre-input-point-at-function:
  (lambda () (thing-at-point 'symbol))
  "Change pre input action. Default is get symbol where cursor at"
  :group 'swoop
  :type 'symbol)
(defun swoop-pre-input (&optional $resume)
  (let ($results)
    (if $resume
        (setq $results swoop-last-query-plain)
      (setq $results (cond (mark-active
                            (buffer-substring-no-properties
                             (region-beginning) (region-end)))
                           ((funcall swoop-pre-input-point-at-function:))
                           (t nil)))
      (deactivate-mark))
    $results))

;;;###autoload
(defun swoop (&optional $query)
  (interactive)
  (if current-prefix-arg
      (swoop-core :$resume t :$query swoop-last-query-plain)
    (swoop-core :$query (or $query (swoop-pre-input)))))
;;;###autoload
(defun swoop-multi (&optional $query)
  (interactive)
  (if current-prefix-arg
      (swoop-core :$resume t :$query swoop-last-query-plain :$multi t)
    (swoop-core :$query (or $query (swoop-pre-input)) :$multi t)))
;;;###autoload
(defun swoop-pcre-regexp (&optional $query)
  (interactive)
  (let ((swoop-use-pcre t))
    (if current-prefix-arg
        (swoop-core :$resume t :$query swoop-last-query-plain)
      (swoop-core :$query (or $query (swoop-pre-input))))))
;;;###autoload
(defun swoop-migemo (&optional $query)
  (interactive)
  (let ((swoop-use-migemo t))
    (if current-prefix-arg
        (swoop-core :$resume t :$query swoop-last-query-plain)
      (swoop-core :$query (or $query (swoop-pre-input))))))
;;;###autoload
(defun swoop-line-length-over80 ()
  (interactive)
  (swoop-core :$query "^[^\n]\\{80,\\}"))
;;;###autoload
(defun swoop-from-isearch ()
  (interactive)
  (swoop :$query (if isearch-regexp
                     isearch-string
                   (regexp-quote isearch-string))))
;;(define-key isearch-mode-map (kbd "C-o") 'helm-swoop-from-isearch)

(defun swoop-multi-from-swoop ()
  (interactive)
  (let (($last-query (format "%s" (minibuffer-contents))))
    (run-with-timer
     0 nil
     (lambda ($q)
       (swoop-core :$query $q :$multi t))
     $last-query)
    (exit-minibuffer)))
(define-key swoop-map (kbd "C-o") 'swoop-multi-from-swoop)

(defun swoop-update ($query $multi)
  (when (get-buffer swoop-buffer)
    (swoop-async-kill-process)
    ;; Issue a session ID
    (setq swoop-async-id-latest (symbol-name (cl-gensym)))
    (unless (listp $query)
      (setq $query (split-string $query " " t)))
    (setq swoop-last-query-converted $query)
    (with-current-buffer swoop-buffer
      (if (not $query)
          (swoop-overlay-clear :$to-empty t :$multi $multi)
        (swoop-async-divider $query $multi)))))

(defun swoop-async-checker ($result $tots $pattern $multi)
  (let* (($id (car $result))
         ($check-key (car $id)))
    (if (equal swoop-async-id-latest $check-key)
        (let (($buf (cdr $id))
              ($lines (cdr $result)))
          (let (($v (ht-get swoop-async-pool $buf)))
            (if $v
                (ht-set swoop-async-pool $buf (cons $lines $v))
              (ht-set swoop-async-pool $buf (cons $lines nil))))
          (let (($n (or (ht-get swoop-async-pool "number") 0)))
            (ht-set swoop-async-pool "number" (1+ $n))
            (when (eq $tots (1+ $n))
              (ht-remove swoop-async-pool "number")
              (swoop-render $pattern $multi))))
      (setq swoop-async-id-last swoop-async-id-latest))))

(cl-defun swoop-render ($pattern $multi)
  (swoop-overlay-clear :$multi $multi)
  (setq swoop-last-selected-buffer
        (or (get-text-property (point-at-bol) 'swb)
            swoop--target-buffer))
  (setq swoop-last-selected-line
        (or (get-text-property (point-at-bol) 'swl)
            swoop--target-last-line
            (ht-get (ht-get swoop-buffer-info swoop--target-buffer) "max-line")
            1))
  (erase-buffer)
  (with-selected-window swoop-window
    (let (($cont "")
          ($match-lines-common)
          ($nearest-line))
      (erase-buffer)
      ;; swoop-async-pool ;=> (($parts-id $match-list $content-list) ...)
      (ht-each (lambda ($buf $val)
                 (if (swoop-async-old-session?) (cl-return-from swoop-render))
                 (let (($con "")
                       ($length (length $val)))
                   (if (eq 1 $length)
                       (progn
                         (setq $con (mapconcat 'identity (cddar $val) ""))
                         (if (equal $buf swoop-last-selected-buffer)
                             (setq $match-lines-common (nth 1 (car $val)))))
                     (swoop-mapc $p (cl-sort $val 'string< :key 'car)
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
               swoop-async-pool)
      (insert $cont)
      (setq $nearest-line
            (swoop-nearest-line swoop-last-selected-line $match-lines-common))
      ;; Words overlay
      (swoop-overlay-word $pattern swoop-buffer)
      (if $multi
          (swoop-mapc $b (ht-keys swoop-buffer-info)
            (if (swoop-async-old-session?) (cl-return-from swoop-render))
            (swoop-overlay-word $pattern $b))
        (swoop-overlay-word $pattern swoop--target-buffer))
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
               (swoop-next-line))
              ((eobp)
               (swoop-prev-line)))
        (swoop-line-move 'init)
        (swoop-header-format-line-set
         (get-text-property (point-at-bol) 'swb))))))

(cl-defun swoop-async-divider ($query &optional $multi)
  (with-current-buffer swoop-buffer
    (setq swoop-async-id-last swoop-async-id-latest)
    (let (($pattern (concat "\\(" (mapconcat 'identity $query "\\|") "\\)"))
          ($tots (let (($r 0))
                   (swoop-mapc $n (swoop-buffer-info-get-map "buf-number")
                     (setq $r (+ $n $r)))
                   $r)))
      (setq swoop-last-pattern $pattern)
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
            (if (swoop-async-old-session?) (cl-return-from swoop-async-divider))
            (swoop-async-start
             `(lambda ()
                (fundamental-mode)
                (insert ,(nth $i $buf-sep))
                (cons (cons ,swoop-async-id-latest ,$buf)
                      (cons ,$buf-sep-id
                            (funcall ,swoop-async-get-match-lines-list
                                     ',$query ,(* $i $by)
                                     ,$line-format
                                     ',swoop-face-line-number
                                     ,$buf))))
             (lambda ($result)
               (when (get-buffer swoop-buffer)
                 (with-current-buffer swoop-buffer
                   (swoop-async-checker $result $tot $pattern $multi)))
               )))))
      (when $multi
        (ht-each
         (lambda ($buf $buf-hash)
           (if (swoop-async-old-session?) (cl-return-from swoop-async-divider))
           (let* (($tot         (ht-get $buf-hash "buf-number"))
                  ($buf-sep     (ht-get $buf-hash "buf-separated"))
                  ($by          (ht-get $buf-hash "divide-by"))
                  ($line-format (ht-get $buf-hash "line-format"))
                  ($buf-sep-id))
             (cl-dotimes ($i $tot)
               (setq $buf-sep-id (symbol-name (cl-gensym)))
               (if (swoop-async-old-session?) (cl-return-from swoop-async-divider))
               (swoop-async-start
                `(lambda ()
                   (fundamental-mode)
                   (insert ,(nth $i $buf-sep))
                   (cons (cons ,swoop-async-id-latest ,$buf)
                         (cons ,$buf-sep-id
                               (funcall ,swoop-async-get-match-lines-list
                                        ',$query ,(* $i $by)
                                        ,$line-format
                                        ',swoop-face-line-number
                                        ,$buf))))
                (lambda ($result)
                  (when (get-buffer swoop-buffer)
                    (with-current-buffer swoop-buffer
                      (swoop-async-checker $result $tots $pattern $multi)))
                  )))))
         swoop-buffer-info)))))

;; Minibuffer
(defun swoop-minibuffer-read-from-string ($query $multi)
  (let (($timer nil))
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
                      (when (and (not (equal swoop-minibuf-last-content
                                             $content))
                                 (or
                                  ;; When becomeing empty again
                                  (equal "" $content)
                                  ;; Avoid too many matching
                                  (>= (length $content)
                                      swoop-input-threshold)))
                        ;; Stop old async process
                        (ht-clear! swoop-async-pool)
                        (setq swoop-minibuf-last-content $content)
                        (swoop-update (swoop-convert-input $content) $multi)
                        )))))))
          (read-from-minibuffer
           "Swoop: " (or $query "")
           swoop-map nil swoop-minibuffer-history nil t))
      (when $timer (cancel-timer $timer) (setq $timer nil))
      (setq swoop-last-query-plain swoop-minibuf-last-content)
      (setq swoop-minibuf-last-content "")
      (recenter))))


(provide 'swoop)
;;; swoop.el ends here
