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

(defvar swoop--target-buffer nil)
(defvar swoop--target-window nil)
(defvar swoop--target-buffer-info nil)
(defvar swoop--last-position nil)
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
(defvar swoop-line-number-face 'swoop-line-number-face
  "For pass to async batch")

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









(provide 'swoop-lib)
;;; swoop-lib.el ends here
