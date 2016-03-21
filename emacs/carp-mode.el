
(define-derived-mode carp-mode clojure-mode "Carp"
  "Major mode for the Carp programming language.")

(add-to-list 'auto-mode-alist '("\\.carp\\'" . carp-mode))

(provide 'carp-mode)
