(flycheck-define-checker carp
  "A Carp syntax checker.

See URL `http://github.com/carp-lang/Carp/'."
  :command ("carp" "-b" source)
  :error-patterns
  ;;((error line-start (file-name) ":" line ": error: " (message) line-end))
  ;;((error line-start "At line" line ", column" column "in" (file-name) line-end))
  ((error line-start (message) line-end))
  :modes carp-mode)
