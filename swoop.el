;;; swoop.el --- Peculiar buffer navigation for Emacs -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2014 by Shingo Fukuyama

;; Version: 1.0
;; Author: Shingo Fukuyama - http://fukuyama.co
;; URL: https://github.com/ShingoFukuyama/emacs-swoop
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

;; Feature:
;; * Search words through a whole buffer or across buffers
;; * Highlight target line and matched words
;; * Stick to the nearest line even after update the list
;; * Utilize PCRE (Perl Compatible Regular Expressions) like search
;; * Utilize migemo (Japanese words search command)
;; * Edit matched lines synchronously
;; * Cache buffer information to start quickly
;; * Shrink text size in buffers to view more
;; * and more

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

;; ;; Transition
;; ;; isearch     > press [C-o] > swoop
;; ;; evil-search > press [C-o] > swoop
;; ;; swoop       > press [C-o] > swoop-multi
;; (define-key isearch-mode-map (kbd "C-o") 'swoop-from-isearch)
;; (define-key evil-motion-state-map (kbd "C-o") 'swoop-from-evil-search)
;; (define-key swoop-map (kbd "C-o") 'swoop-multi-from-swoop)

;; ;; Resume
;; ;; C-u M-x swoop : Use last used query

;; ;; Swoop Edit Mode
;; ;; During swoop, press [C-c C-e]
;; ;; You can edit synchronously

;;; TODO:
;; Unpropertize (thing-at-point 'symbol)
;; Prevent long time loop words (], \\b, {0,} ...)

;;; Code:

(require 'swoop-lib)
(require 'swoop-edit)

;; Cache control
(defun swoop-cache-clear ()
  (when (not (ht-empty? swoop-buffer-info))
    (ht-clear! swoop-buffer-info)
    (swoop-async-kill-process)
    (swoop-async-kill-process-buffer)))
(add-hook 'after-save-hook 'swoop-cache-clear)

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
  "Back cursor position to where the last swoop happened."
  (interactive)
  (let (($po swoop--target-last-position))
    (setq swoop--target-last-position (point))
    (with-selected-window swoop--target-window
      (goto-char $po))))
(defun swoop-highlight-for-cancel ()
  "Prevent loosing sight of cursor position. It'll evaporate at once."
  (interactive)
  (let* (($lbeg (point))
         ($lend (point-at-eol))
         ($lov (make-overlay $lbeg $lend))
         ($lbeg2 (point-at-bol))
         ($lend2 $lbeg)
         ($lov2 (make-overlay $lbeg2 $lend2)))
    (swoop-recenter)
    (run-with-timer 0.3 nil (lambda ($o) (delete-overlay $o)) $lov)
    (overlay-put $lov 'face 'swoop-face-target-words)
    (run-with-timer 0.3 nil (lambda ($o) (delete-overlay $o)) $lov2)
    (overlay-put $lov2 'face 'swoop-face-target-line)))
(add-hook 'swoop-abort-hook 'swoop-back-to-last-position)
(add-hook 'swoop-abort-hook 'swoop-highlight-for-cancel)

;; Default action
(defun swoop-action-goto-target-point ()
  "Finish searching and goto the target line"
  (interactive)
  (run-with-timer
   0 nil
   (lambda ($info)
     (with-selected-window swoop--target-window
       (with-current-buffer (cdr $info)
         (set-window-buffer nil (cdr $info))
         (swoop-goto-line (car $info))
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
                ($lov  (make-overlay $lbeg $lend))
                ($wbeg (match-beginning 0))
                ($wend (match-end 0))
                ($wov  (make-overlay $wbeg $wend)))
           (run-with-timer 0.28 nil (lambda ($o) (delete-overlay $o)) $lov)
           (overlay-put $lov 'face 'swoop-face-target-words)
           (run-with-timer 0.4 nil (lambda ($o) (delete-overlay $o)) $wov)
           (overlay-put $wov 'face 'swoop-face-target-line)
           (swoop-recenter)))))
   (with-selected-window swoop-window
     (cons (get-text-property (point) 'swl) (get-text-property (point) 'swb))))
  (exit-minibuffer))

;; Overlay
(cl-defun swoop-overlay-clear (&key $to-empty $kill $multi)
  "Clear overlays, and kill swoop-buffer"
  (if (and $kill (get-buffer swoop-buffer))
      (kill-buffer swoop-buffer))
  (if swoop-use-target-magnifier:
      (swoop-magnify-around-target :$delete t))
  (if (and swoop-overlay-target-buffer-selection
           (not $to-empty))
      (delete-overlay swoop-overlay-target-buffer-selection))
  (when (and swoop-font-size-change:
             $kill
             (not $multi)
             (eq 1 (length swoop-overlay-target-buffer)))
    (delete-overlay (car swoop-overlay-target-buffer))
    (setq swoop-overlay-target-buffer nil))
  (when (and swoop-font-size-change:
             $kill
             $multi
             (< 1 (length swoop-overlay-target-buffer)))
    (swoop-mapc $ov swoop-overlay-target-buffer
      (delete-overlay $ov))
    (setq swoop-overlay-target-buffer nil))
  (swoop-mapc $buf (if $multi
                       (ht-keys swoop-buffer-info)
                     (list swoop--target-buffer))
    (if (and (swoop-async-old-session?)
             (not $to-empty)
             (not $kill))
        (cl-return-from swoop-overlay-clear))
    (with-current-buffer $buf
      (overlay-recenter (point-max))
      (swoop-mapc $ov (overlays-in (point-min) (point-max))
        (if (and (swoop-async-old-session?)
                 (not $to-empty)
                 (not $kill))
            (cl-return))
        (when (overlay-get $ov 'swoop-temporary)
          (delete-overlay $ov))))))
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



(cl-defun swoop-core (&key $query $reserve $resume $multi $pre-select)
  (setq
   swoop--target-last-position (point)
   swoop--target-last-line (line-number-at-pos)
   swoop--target-buffer (buffer-name (current-buffer))
   swoop--target-window (get-buffer-window swoop--target-buffer))
  (if $multi
      (swoop-set-buffer-info-all)
    (swoop-set-buffer-info swoop--target-buffer))
  (swoop-overlay-font-size-change $multi)
  (swoop-overlay-selection-target-buffer-set)
  (setq swoop-parameters
        (ht ("reserve" $reserve)
            ("pcre" swoop-use-pcre)
            ("migemo" swoop-use-migemo)))
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
          (when $resume
            ;; Prevent following minibuffer session once
            (setq swoop-minibuf-last-content $query)
            ;; First time
            (swoop-update :$query swoop-last-query-converted
                          :$reserve $reserve
                          :$multi $multi
                          :$pre-select $pre-select))
          (when (or $reserve $pre-select)
            (swoop-update :$query (or $query "")
                          :$reserve $reserve
                          :$multi $multi
                          :$pre-select $pre-select))
          (swoop-minibuffer-read-from-string :$query $query
                                             :$reserve $reserve
                                             :$multi $multi
                                             :$pre-select $pre-select))
      (when (get-buffer swoop-buffer)
        (swoop-overlay-clear :$kill t :$multi (or $multi nil)))
      ;; Restore last position of other buffers
      (when $multi
        (swoop-mapc $buf (ht-keys swoop-buffer-info)
          (unless (equal $buf (buffer-name (current-buffer)))
            (goto-char (swoop-buffer-info-get $buf "point")))))
      (setq swoop-use-pcre nil)
      (setq swoop-use-migemo nil)
      (setq swoop-match-beginning-line nil)
      (ht-clear! swoop-parameters))))

(defcustom swoop-pre-input-point-at-function:
  (lambda ()
    (let ((query (thing-at-point 'symbol)))
      (if query
          (format "%s" (read query))
        "")))
  "Change pre input action. Default is get symbol where cursor at."
  :group 'swoop :type 'symbol)
(defun swoop-pre-input (&optional $resume)
  "Pre input function. Utilize region and at point symbol"
  (let ($results)
    (if $resume
        (setq $results swoop-last-query-plain)
      (setq $results (cond (mark-active
                            (buffer-substring-no-properties
                             (region-beginning) (region-end)))
                           ((funcall swoop-pre-input-point-at-function:))
                           (t nil)))
      (deactivate-mark)
      (when $results
        (setq $results (replace-regexp-in-string "\*" "\\\\*" $results))
        (setq $results (replace-regexp-in-string "\+" "\\\\+" $results))))
    $results))

;;;###autoload
(defun swoop (&optional $query)
  "Search through words within the current buffer."
  (interactive)
  (if current-prefix-arg
      (swoop-core :$resume t :$query swoop-last-query-plain)
    (swoop-core :$query (or $query (swoop-pre-input)))))
;;;###autoload
(defun swoop-multi (&optional $query)
  "Search words across currently opened multiple buffers.
Ignore non file buffer."
  (interactive)
  (if current-prefix-arg
      (swoop-core :$resume t :$query swoop-last-query-plain :$multi t)
    (swoop-core :$query (or $query (swoop-pre-input)) :$multi t)))
;;;###autoload
(defun swoop-pcre-regexp (&optional $query)
  "Use PCRE like regexp to swoop."
  (interactive)
  (setq swoop-use-pcre t)
  (if current-prefix-arg
      (swoop-core :$resume t :$query swoop-last-query-plain)
    (swoop-core :$query (or $query (swoop-pre-input)))))
;;;###autoload
(defun swoop-migemo (&optional $query)
  "Japanese words matching with the alphabet."
  (interactive)
  (setq swoop-use-migemo t)
  (if current-prefix-arg
      (swoop-core :$resume t :$query swoop-last-query-plain)
    (swoop-core :$query (or $query (swoop-pre-input)))))
;;;###autoload
(defun swoop-line-length-over80 ()
  "Get over 80 colomn number linees."
  (interactive)
  (swoop-core :$reserve "^[^\n]\\{80,\\}"))
;;;###autoload
(defun swoop-from-isearch ()
  "During isearch, switch over to swoop."
  (interactive)
  (swoop-core :$query (if isearch-regexp
                          isearch-string
                        (regexp-quote isearch-string))))
;; (define-key isearch-mode-map (kbd "C-o") 'swoop-from-isearch)

;;;###autoload
(defun swoop-function (&optional $query)
  "Show function list in buffer judging from major-mode and regexp.
Currently c-mode only."
  (interactive)
  (setq swoop-match-beginning-line t)
  (swoop-core :$query (or $query (swoop-pre-input))
              :$reserve
              (cl-case major-mode
                (c-mode
                 (concat
                  "^[[:alnum:]]+\\s-\\([[:alnum:]_:<>~]+\\s-*\\)+"
                  "\\([^)]\\|\\s-\\)+)[^;]")))))

(defun swoop-multi-from-swoop ()
  "During swoop, switch over to swoop-multi."
  (interactive)
  (let (($last-query  swoop-minibuf-last-content)
        ($reserve     (ht-get swoop-parameters "reserve"))
        ($pcre        (ht-get swoop-parameters "pcre"))
        ($migemo      (ht-get swoop-parameters "migemo")))
    (run-with-timer
     0 nil
     (lambda ($q)
       (cond ($pcre (setq swoop-use-pcre t))
             ($migemo (setq swoop-use-migemo t)))
       (swoop-core :$query $q
                   :$resume t
                   :$multi t
                   :$reserve $reserve))
     $last-query)
    (exit-minibuffer)))
;; (define-key swoop-map (kbd "C-o") 'swoop-multi-from-swoop)

;;;###autoload
(defun swoop-from-evil-search ()
  "During evil-search, switch over to swoop."
  (interactive)
  (if (string-match "\\(isearch-\\|evil.*search\\)"
                    (symbol-name real-last-command))
      (swoop-core :$query (if isearch-regexp
                              isearch-string
                            (regexp-quote isearch-string)))
    (swoop)))
;; (define-key evil-motion-state-map (kbd "C-o") 'swoop-from-evil-search)

(cl-defun swoop-update (&key $query $reserve $multi $pre-select)
  (when (get-buffer swoop-buffer)
    (swoop-async-kill-process)
    ;; Issue a session ID
    (setq swoop-async-id-latest (symbol-name (cl-gensym)))
    (unless (listp $query)
      (setq $query (split-string $query " " t)))
    (setq swoop-last-query-converted $query)
    (with-current-buffer swoop-buffer
      (if (and (not $query) (not $pre-select) (not $reserve))
          (swoop-overlay-clear :$to-empty t :$multi $multi)
        (swoop-async-divider :$query $query
                             :$reserve $reserve
                             :$multi $multi
                             :$pre-select $pre-select)))))

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
              (swoop-render $pattern $multi)))))))

(cl-defun swoop-render ($pattern $multi)
  "Rendering results, and repositioning the selected line."
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
                         (setq $con (mapconcat 'identity
                                               (cdr (cdr (car $val))) ""))
                         (if (equal $buf swoop-last-selected-buffer)
                             (setq $match-lines-common (nth 1 (car $val)))))
                     (swoop-mapc $p (cl-sort $val 'string< :key 'car)
                       (unless (equal $p "")
                         (setq $con (concat $con (mapconcat 'identity (cddr $p) "")))
                         (if (equal $buf swoop-last-selected-buffer)
                             (setq $match-lines-common
                                   (append (nth 1 $p) $match-lines-common))))))
                   (when (not (equal "" $con))
                     (setq $con
                           (concat
                            (propertize
                             (concat $buf "\n")
                             'face 'swoop-face-line-buffer-name
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
               (swoop-action-goto-line-next))
              ((eobp)
               (swoop-action-goto-line-prev)))
        (swoop-line-move 'init)
        (swoop-header-format-line-set
         (get-text-property (point-at-bol) 'swb))))))

(cl-defun swoop-async-divider (&key $query $reserve $multi $pre-select)
  "Divide buffers for async process."
  (with-current-buffer swoop-buffer
    (setq swoop-async-id-last swoop-async-id-latest)
    (let (($pattern (concat "\\(" (mapconcat 'identity $query "\\|") "\\)"))
          ($tots (let (($r 0))
                   (swoop-mapc $n (swoop-buffer-info-get-map "buf-number")
                     (setq $r (+ $n $r)))
                   $r)))
      (when $reserve
        (setq $query (cons $reserve $query)))
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
                                     ',swoop-n
                                     ,$buf
                                     ',$pre-select
                                     ,swoop-match-beginning-line))))
             (lambda ($result)
               (when (get-buffer swoop-buffer)
                 (with-current-buffer swoop-buffer
                   (swoop-async-checker $result $tot $pattern $multi)))
               )))))

      (when $multi
        (ht-each
         (lambda ($b $buf-hash)
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
                   (cons (cons ,swoop-async-id-latest ,$b)
                         (cons ,$buf-sep-id
                               (funcall ,swoop-async-get-match-lines-list
                                        ',$query ,(* $i $by)
                                        ,$line-format
                                        ',swoop-n
                                        ,$b
                                        ',$pre-select
                                        ,swoop-match-beginning-line))))
                (lambda ($result)
                  (when (get-buffer swoop-buffer)
                    (with-current-buffer swoop-buffer
                      (swoop-async-checker $result $tots $pattern $multi)))
                  )))))
         swoop-buffer-info)))))

;; Minibuffer
(cl-defun swoop-minibuffer-read-from-string (&key $query $reserve $multi $pre-select)
  "Observe minibuffer inputs."
  (if (equal "" $query) (setq swoop-minibuf-last-content nil))
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
                        (swoop-update :$query (swoop-convert-input $content)
                                      :$reserve $reserve
                                      :$multi $multi
                                      :$pre-select $pre-select)
                        )))))))
          (read-from-minibuffer
           "Swoop: " (or $query "")
           swoop-map nil swoop-minibuffer-history nil t))
      (when $timer (cancel-timer $timer) (setq $timer nil))
      (setq swoop-last-query-plain swoop-minibuf-last-content)
      (setq swoop-minibuf-last-content "")
      (swoop-recenter))))


(provide 'swoop)
;;; swoop.el ends here
