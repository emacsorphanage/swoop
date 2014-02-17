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

(require 'async)
(require 'pcre2el)
(require 'ht)

(defgroup swoop nil
  "Group for swoop"
  :prefix "swoop-" :group 'convenience)

(defvar swoop-buffer "*Swoop*")
(defvar swoop-window nil)
(defvar swoop-buffer-selection-overlay nil)
(defvar swoop-target-buffer-overlay nil)
(defvar swoop-target-buffer-selection-overlay nil)
(defvar swoop-last-selected-buffer nil)
(defvar swoop-last-selected-line nil)
(defvar swoop-buffer-info (ht-create 'equal))
(defvar swoop-minibuffer-input-dilay 0)
(defvar swoop-input-threshold 2)
(defvar swoop-minibuffer-history nil)

(defvar swoop--target-buffer nil)
(defvar swoop--target-window nil)
(defvar swoop--target-buffer-info nil)
(defvar swoop--last-position nil)
(defvar swoop--last-line nil)
(defvar swoop--minibuf-last-content nil)
(defvar swoop--last-query-plain nil)
(defvar swoop--last-query-converted nil)

(defvar swoop-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "C-n") 'swoop--next-line)
    (define-key map (kbd "C-p") 'swoop--prev-line)
    (define-key map (kbd "C-g") 'swoop--cancel)
    (define-key map (kbd "RET") 'swoop--default-action)
    (define-key map (kbd "<C-return>") 'swoop--default-action)
    map))

(defface swoop-target-line-face
  '((t :background "#e3e300" :foreground "#222222"))
  "Target line face"
  :group 'swoop)
(defface swoop-target-words-face
  '((t :background "#7700ff" :foreground "#ffffff"))
  "Target words face"
  :group 'swoop)
(defface swoop-header-line-face
  '((t :height 1.3 :foreground "#999999" :weight bold))
  "Currently selecting buffer name which appears on the header line"
  :group 'swoop)
(defface swoop-line-buffer-name-face
  '((t :height 1.5 :background "#333333" :foreground "#eeeeee" :weight bold))
  "Buffer name line face"
  :group 'swoop)
(defface swoop-line-number-face
  '((t :foreground "#ff9900"))
  "Line number face"
  :group 'swoop)
(defvar swoop-line-number-face 'swoop-line-number-face
  "For pass to async batch")

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

(defsubst swoop--goto-line ($line)
  (goto-char (point-min))
  (forward-line (1- $line)))
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

;; (defun swoop--get-point-from-line ($line &optional $buf)
;;   (or $buf (setq $buf (current-buffer)))
;;   (save-excursion
;;     (with-current-buffer $buf
;;       (swoop--goto-line $line)
;;       ;; Must subtract 1 for extract buffer contents,
;;       ;; by substring-no-properties
;;       (1- (point)))))

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
                  (1- (save-excursion (swoop--goto-line (1+ (* $i $by))) (point)))
                  ;; To show the last line
                  (if (>= (* (1+ $i) $by) $max-line)
                      nil
                    (1- (save-excursion
                          (swoop--goto-line (1+ (* (1+ $i) $by))) (point)))))
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

;; (defsubst swoop--hash-values-to-list ($hash)
;;   (let ($results)
;;     (maphash (lambda (ignored $val)
;;                (setq $results (cons $val $results))) $hash)
;;     $results))

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
      ;; (unless (and (member $buf (ht-keys swoop-buffer-info))
      ;;              ;;(not (with-current-buffer $buf (buffer-modified-p)))
      ;;              )
        (swoop--set-buffer-info $buf)
        ;; )
      )
    (swoop--mapc $buf (ht-keys swoop-buffer-info)
      (unless (member $buf $bufs)
        (ht-remove! swoop-buffer-info $buf)))))

(defun swoop--buffer-info-get ($buf $key2)
  (ht-get (ht-get swoop-buffer-info $buf) $key2))

(defun swoop--buffer-info-get-map ($key)
  (ht-map (lambda ($bname $binfo)
            (ht-get $binfo $key))
          swoop-buffer-info))


;;(swoop--nearest-line 50 '(10 90 20 80 30 40 45 56 70))
(defun swoop--nearest-line ($target $list)
  (when (and $target $list)
    (let ($result)
      (cl-flet ((filter ($fn $elem $list)
                        (let ($r)
                          (mapc (lambda ($e)
                                  (if (funcall $fn $elem $e)
                                      (setq $r (cons $e $r))))
                                $list) $r)))
        (if (eq 1 (length $list))
            (setq $result (car $list))
          (let* (($lt (car
                       (sort (filter '> $target $list) '>)))
                 ($gt (car
                       (sort (filter '< $target $list) '<)))
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



(provide 'swoop-lib)
;;; swoop-lib.el ends here
