# Swoop.el

Peculiar buffer navigation for Emacs.

![swoop.el](https://raw2.github.com/ShingoFukuyama/images/master/swoop.gif)

## Feature

* Search words through a whole buffer or across buffers
* Highlight target line and matched words
* Stick to the nearest line even after update the list
* Utilize PCRE (Perl Compatible Regular Expressions) like search
* Utilize migemo (Japanese words search command)
* Edit matched lines synchronously
* Cache buffer information to start quickly
* Shrink text size in buffers to view more
* and more

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

## Options

### Window configuration
```
;; t:   Show swoop lines within the current window
;; nil: Show swoop lines in another window
(setq swoop-window-split-current-window: nil)
;; Determine the split direction 'split-window-horizontally or 'split-window-vertically
(setq swoop-window-split-direction: 'split-window-vertically)
```


### Font size change
```
;; Change whole buffer's font size (t or nil)
(setq swoop-font-size-change: t)
;; Font size (e.g. 0.8, 1.0, 1.5, 80, 135)
(setq swoop-font-size: 0.9)
```


### Magnify around target line
```
;;
;; Enable around target lines magnifier (t or nil)
(setq swoop-use-target-magnifier: t)
;; Magnify area from target line
(setq swoop-use-target-magnifier-around: 10)
;; Font size for magnify area (e.g. 0.8, 1.0, 1.5, 80, 135)
(setq swoop-use-target-magnifier-size: 1.2)
```


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
