;;; swoop-async.el --- Peculiar buffer navigation for Emacs -*- coding: utf-8; lexical-binding: t -*-

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

(defvar swoop--async-pool (make-hash-table :test 'equal))
(defvar swoop--async-id-latest nil)
(defvar swoop--async-id-last nil)
(defvar swoop--async-get-match-lines-list
  "Byte compiled function. It works in async process.")


(defsubst swoop--old-session? ()
  (not (equal swoop--async-id-last swoop--async-id-latest)))

(defmacro swoop--async-start ($start-func $finish-func)
  (require 'find-func)
  (let ((procvar (make-symbol "proc")))
    `(let* ((sexp ,$start-func)
            (,procvar
             (swoop--async-start-process
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

(defun swoop--async-start-process (name program finish-func &rest program-args)
  "Start the executable PROGRAM asynchronously.  See `async-start'.
PROGRAM is passed PROGRAM-ARGS, calling FINISH-FUNC with the
process object when done.  If FINISH-FUNC is nil, the future
object will return the process object when the program is
finished."
  (let* ((buf (generate-new-buffer (concat "*" name "*")))
         (proc (let ((process-connection-type nil))
                 (apply #'start-process name buf program program-args))))
    (with-current-buffer buf
      (set (make-local-variable 'async-callback) finish-func)
      (set-process-sentinel proc #'async-when-done)
      (unless (string= name "swoop-batch")
        (set (make-local-variable 'async-callback-for-process) t))
      proc)))

(defun swoop--kill-process-buffer ()
  (mapc (lambda ($buf)
          (setq $buf (buffer-name $buf))
          (when (string-match "^\\*swoop-batch\\*" $buf)
            (let ((kill-buffer-query-functions nil))
              (kill-buffer $buf))))
        (buffer-list)))

(defun swoop--kill-process ()
  (mapc (lambda ($proc)
          (when (string-match "swoop-batch" (process-name $proc))
            (delete-process $proc)))
        (process-list)))

(defun swoop--async-get-match-lines-list
  ($query $from $line-format $line-face $buf)
  ;; Prevent "Odd length text property list" error
  (setq vc-handled-backends nil)
  (save-excursion
    (let* (($lines nil)
           ($pos-min (point-min))
           ($pos-max (point-max))
           (buffer-invisibility-spec nil)
           ($match-lines)
           ($match-total)
           ($match-lines-common))
      (goto-char $pos-min)
      (put-text-property $pos-min $pos-max 'swb $buf)
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
        (setq $match-lines-common $results))

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
      (cons $match-lines-common $lines))))
(setq swoop--async-get-match-lines-list
      (byte-compile 'swoop--async-get-match-lines-list))


(provide 'swoop-async)
;;; swoop-async.el ends here
