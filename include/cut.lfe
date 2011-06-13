(include-file "gensym.lfe")
;; Fun specialization macro. Stands for "curry up this".
;;
;; Expanding (cut a b c ...) creates a fun that applies (a b c ...), 
;; filling in <> elements with arguments. The (cute a b c ...) variant
;; evaluates known arguments before the fun is created, rather than
;; delaying evaluation until the fun is called.
;; 
;; Uses gensym to create unique symbol names, so no collisions. Only
;; replaces on a single level, so you can nest cuts.
;;
;;      Examples:
;;
;; Well suited to folds, maps, and zipwiths:
;;  (map (cut + 1 <>) vs)     -> (map (lambda (x) (+ 1 x)) vs)
;;  (filter (cut >= 0 <>) vs) -> (filter (lambda (x) (>= 0 x) vs)
;;
;; Can be used to specialize complex calls into simpler ones:
;;  (cut foo a b <> c <> d)   -> (lambda (x y) (foo a b x c y d))
;;
;; The "e" variant evaluates arguments when the fun is created, rather than
;; when it's called. This is a concise way to lift expensive operations out of
;; a loop, or precompute values:
;;  (map (cute dialyze-module <> (build-plt modules)) modules)
;;  (defun float-re
;;    (cute re:run <> (element 2 (re:compile '"[+-]?[0-9]*\\.?[0-9]*"))))
;; 
;;      Motivation:
;;
;; Most uses of lambdas are very simple, and don't need their full power. Cuts
;; provide an appropriately simple syntax, the same way map and filter are much
;; simpler than identical C-style for(;;) loops or recursive pattern matches.
(defmacro cut (es
  (let* ((symbols (lc ((<- e es)) 
                      (if (== e '<>) 
                        (gensym 'cut) 
                        'pass)))
         (args (lc ((<- s (when (/= 'pass s)) symbols)) 
                   s))
         (replace (lambda (s e)
                   (if (/= s 'pass) s e)))
         (body (: lists zipwith replace symbols es)))
    `(lambda ,args ,body))))

(defmacro cute (es
  (let* ((types (lc ((<- e es)) 
                    (cond ((== '<> e)  (tuple 'arg   (gensym 'cute)))
                          ((is_list e) (tuple 'eval  (gensym 'cute) e))
                          ('true       (tuple 'const e)))))
         (body (lc ((<- t types)) 
                   (element 2 t)))
         (args (lc ((<- t (when (== 'arg (element 1 t))) types)) 
                   (element 2 t)))
         (evals (lc ((<- t (when (== 'eval (element 1 t))) types))
                    `(,(element 2 t) ,(element 3 t)))))
    (if (== '[] evals) 
      fun ; cut-equivalent
      `(let ,evals (lambda ,args ,body))))))
