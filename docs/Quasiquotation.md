# Quasiquotation

Quasiquotation is a way to quote parts of a list while evaluating others. It’s
only available in the dynamic parts of the program.

Quasiquotation enables the user to intersperse evaluated or “unquoted” portions
into a unevaluated or “quoted” list.

```clojure
(defdynamic x 2)

(quasiquote (+ (unquote x) 1)) ; => (+ 2 1)

; also available as literals, quasiquote becomes `
; and unquote becomes %
`(+ %x 1) ; => (+ 2 1)
```

Note that unquoting only makes sense inside `quasiquote` forms and using it
outside will lead to errors at macro expansion time.

Since quasiquotation primarily deals with lists, the user might sometimes want
to intersperse another list of values flatly, “splicing” them in. For this case
Carp provides `unquote-splicing`.

```clojure
(defdynamic x '(1 2))

(quasiquote (+ (unquote-splicing x))) ; => (+ 1 2)

; the literal for unquote-splicing is %@
`(+ %@x) ; => (+ 1 2)
```

Please note that while the code examples above only use variables, any
expression can be used inside the `unquote` variants.

```clojure
(quasiquote (+ (unquote-splicing (map inc [1 2])))) ; => (+ 2 3)
; or
`(+ %@(map inc [1 2])) ; => (+ 2 3)
```
