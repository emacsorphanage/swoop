# Swoop.el

Peculiar buffer navigation for Emacs.

## Config example

```
(require 'swoop)
(global-set-key (kbd "C-o")   'swoop)
(global-set-key (kbd "C-M-o") 'swoop-multi)
(global-set-key (kbd "M-o")   'swoop-pcre-regexp)
(global-set-key (kbd "C-S-o") 'swoop-back-to-last-position)
(global-set-key (kbd "H-6")   'swoop-migemo) ;; Option for Japanese match
```

## Swoop Edit Mode
During swoop, press [C-c C-e]  
You can edit buffers synchronously.

## Transition

```
;; isearch     > press [C-o] > swoop
;; evil-search > press [C-o] > swoop
;; swoop       > press [C-o] > swoop-multi
(define-key isearch-mode-map (kbd "C-o") 'swoop-from-isearch)
(define-key evil-motion-state-map (kbd "C-o") 'swoop-from-evil-search)
(define-key swoop-map (kbd "C-o") 'swoop-multi-from-swoop)
```
swoop > swoop-multi can also inherit PCRE or migemo condition.

## Resume
Use last used query by pressing C-u M-x swoop


## Require other elisp packages

async.el    https://github.com/jwiegley/emacs-async

pcre2el.el  https://github.com/joddie/pcre2el

ht.el       https://github.com/Wilfred/ht.el

## Option for Japanese match by cmigemo command

```
;; Install cmigemo command
From https://github.com/koron/cmigemo  

If you use homebrew on Mac OSX  
`brew install cmigemo`

;; Specify the migemo-dict place in your system.
(defvar swoop-migemo-options
  "-q -e -d /usr/local/share/migemo/utf-8/migemo-dict")
```
