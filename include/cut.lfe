(include-file "gensym.lfe")
;; Fun specialization macro. Stands for "curry up this".
;;
;; Well suited to folds, maps, zipwiths, and a ton of other higher order
;; functions. Examples:
;;
;; (map (cut + 1 <>) vs) -> (map (lambda (x) (+ 1 x)) vs)
;; (filter (cut >= 0 <>) vs) -> (filter (lambda (x) (>= 0 x) vs)
;; (cut foo a b <> c <> d) -> (lambda (x y) (foo a b x c y d))
;; 
;; Actually uses gensym to create unique symbol names, no collisions. Only
;; replaces on a single level, so you can nest cuts.
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
