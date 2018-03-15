(require 'flycheck)

(flycheck-define-checker carp-checker
  "A Carp syntax checker.

See URL `http://github.com/carp-lang/Carp/'."
  :command ("carp" "--check" source)
  :error-patterns ((error line-start space (message) "\n  At line " line ", column " column " in '" (file-name) "'" line-end)
                   (error line-start (file-name) ":" line ":" column " " (message) line-end))
  :modes (carp-mode))

(eval-after-load 'flycheck
  '(add-to-list 'flycheck-checkers 'carp-checker))

;; (flycheck-parse-error-with-patterns
;;  "  At line 11, column 3 in 'foo.carp'"
;;  (flycheck-checker-get 'carp-checker 'error-patterns)
;;  'carp-checker)

(provide 'carp-flycheck)
