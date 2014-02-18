;;; swoop-edit.el --- Peculiar buffer navigation for Emacs -*- coding: utf-8; lexical-binding: t -*-

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

(require 'swoop-lib)

(defvar swoop-edit-buffer "*Swoop Edit*")

(defvar swoop-edit-map
  (let (($map (make-sparse-keymap)))
    ;; (define-key $map (kbd "C-x C-s") 'swoop-edit-apply-changes)
    (define-key $map (kbd "C-x C-s") 'swoop-edit-finish)
    (define-key $map (kbd "C-c C-c") 'swoop-edit-finish)
    $map))
(define-key swoop-map (kbd "C-c C-e") 'swoop-edit)

(defun swoop-edit-finish ()
  "Kill the edit buffer. Changes should have already been applied."
  (interactive)
  (select-window swoop--target-window)
  (with-current-buffer swoop--target-buffer
    (set-window-buffer nil swoop--target-buffer)
    (goto-char swoop--target-last-position))
  (kill-buffer swoop-edit-buffer))

(defun swoop-modify-buffer-content ($bufcont)
  "Modify the original buffer content, but it causes slow rendering."
  $bufcont)

(defsubst swoop-line-beg-point ($line &optional $buf)
  (with-current-buffer (or $buf (current-buffer))
    (save-excursion
      (swoop-goto-line $line) (point))))

(defsubst swoop-set-marker ($line &optional $buf)
  (with-current-buffer (or $buf (current-buffer))
    (save-excursion
      (swoop-goto-line $line)
      (set-marker (make-marker) (point)))))

(defun swoop-edit ()
  "Modify matched lines. Changes are automatically applying to target buffers."
  (interactive)
  (let (($bufcont (with-current-buffer swoop-buffer
                    (buffer-substring
                     (point-min) (point-max)))))
    (run-with-timer
     0 nil
     (lambda ($bufcont $bufname)
       (when (get-buffer swoop-edit-buffer)
         (kill-buffer swoop-edit-buffer))
       (funcall swoop-display-function swoop-edit-buffer)
       (erase-buffer)
       ;; Header
       (insert (propertize
                (concat " " $bufname "\n")
                'face
                'swoop-line-buffer-name-face
                'intangible t))
       ;; Body
       (insert $bufcont)
       (save-excursion
         (goto-char (point-min))
         (add-text-properties (point-min) (point-max)
                              '(read-only t rear-nonsticky t front-sticky t))
         (let (($linum)
               (inhibit-read-only t))
           (goto-char (point-min))
           (while (not (eobp))
             (goto-char (or (next-single-property-change (point) 'swl) (point-max)))
             (when (setq $linum (get-text-property (point) 'swl))
               (let (($line-buf (get-text-property (point) 'swb)))
                 (insert (propertize
                          (format "%s:: " $linum)
                          'swp t
                          'face 'swoop-face-line-number
                          'intangible t
                          'rear-nonsticky t
                          'read-only t))
                 (put-text-property
                  (point-at-bol) (point-at-eol)
                  'swm (save-excursion
                         (with-current-buffer $line-buf
                           (swoop-goto-line $linum)
                           (set-marker
                            (make-marker)
                            (point)))))
                 (remove-text-properties (point) (point-at-eol) '(read-only t))
                 (set-text-properties (point-at-eol) (1+ (point-at-eol))
                                      '(read-only t rear-nonsticky t)))))))
       (swoop-overlay-word swoop-last-pattern (current-buffer))
       (goto-char (point-min))
       (forward-line 1)
       (re-search-forward "^[[:space:]]*\\([0-9]+\\)::[[:space:]]" nil t)
       (add-hook 'after-change-functions 'swoop-edit-sync nil t)
       (use-local-map swoop-edit-map))
     ;; Args
     $bufcont
     swoop--target-buffer)
    (exit-minibuffer)))

(defun swoop-edit-sync ($beg $end $length)
  (save-excursion
    (goto-char $beg)
    (let* (($line-beg (point-at-bol))
           ($marker (get-text-property $line-beg 'swm))
           ($buf (marker-buffer $marker))
           $col)
      (when (and (get-text-property $line-beg 'swp)
                 (not (get-text-property $end 'swp)))
        (when (= $length 0)
          (put-text-property $beg $end 'swm $marker)
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
                                   $line-beg 'swp nil
                                   $line-end))
                       (setq $col (- (point) $line-beg))
                       (buffer-substring-no-properties (point) $line-end))))
          (with-selected-window $win
            (goto-char $marker)
            ;; Unveil invisible block
            (swoop-mapc $ov
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


(provide 'swoop-edit)
;;; swoop-edit.el ends here
