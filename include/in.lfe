;; Declarative, pure function composition. Haskell 'where' analogue.
;;
;;  MOTIVATION:
;;
;; Procedural code focuses on sequencing side effects, and the syntax reflects
;; that focus. Functional code has no side effects, so there is no order of
;; operations. The order you compose functions in, is less restricted.
;;
;; This allows functions to be written more naturally, in a top-down manner
;; leaving the gritty details to the end.
;;
;;  IMPLEMENTATION: 
;;
;; I am lazy, so for now this is a fully-reversed let. I'll try and add
;; dependency identification later (pattern matches make it a little tricky,
;; but doable) so any order may be used.
;;
;;  EXAMPLE:
;;
;; (defun init [_]
;;   (in (tuple 'ok (tuple restart children))
;;       [restart (tuple 'simple_one_for_one 5 5)
;;        children (list foo_serv)
;;        foo_serv (tuple mod start 'permanent 5000 'worker (list mod))
;;        start (tuple mod 'start_link '())
;;        mod 'foo_serv]))
;;
(defmacro in ((list sexp bindings) 
  (fletrec [(even [es] 
                  (if (== 0 (length es))
                    '()
                    (cons (list (car es) (cadr es))
                          (even (cddr es)))))]
    `(let* ,(: lists reverse (even bindings)) ,sexp))))
