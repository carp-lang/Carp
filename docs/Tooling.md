# Tooling

## Vim
[https://github.com/hellerve/carp-vim](https://github.com/hellerve/carp-vim)

## Emacs
This repo contains a hacked-together Emacs package that depends on Clojure mode being installed.
It will hopefully be rewritten and put into one of the package managers eventually, for now you can do something like this to config it:

```
(add-to-list 'load-path "~/Projects/Carp/emacs")

(require 'carp-mode)
(require 'inf-carp-mode)

(add-to-list 'auto-mode-alist '("\\.carp\\'" . carp-mode))
(setq inferior-lisp-program "carp")
(add-hook 'carp-mode-hook
          (lambda ()
            (electric-pair-local-mode 0)
            (smartparens-mode 1)))
```

## Other editors
Clojure syntax highlighting works very well with Carp since it uses the same symbols for most things.
If you have written an editor mode, please tell us and it will be added here!
