# Now on Developing

Peculiar buffer navigation for Emacs.

## Require other elisp packages

async.el    https://github.com/jwiegley/emacs-async

pcre2el.el  https://github.com/joddie/pcre2el


## Example config

```
(require 'swoop)
(global-set-key (kbd "C-o")   'swoop)
(global-set-key (kbd "M-o")   'swoop-pcre-regexp)
(global-set-key (kbd "C-S-o") 'swoop-back-to-last-position)
(global-set-key (kbd "H-6")   'swoop-migemo)
```

## Swoop Edit Mode
During swoop, press [C-c C-e]  
You can edit synchronously.
