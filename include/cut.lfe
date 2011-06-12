(include-file "gensym.lfe")

(defmacro cut (es
  (let* ((symbols (lc ((<- e es)) 
                      (if (== e '<>) 
                        (gensym) 
                        'pass)))
         (args (lc ((<- s (when (/= 'pass s)) symbols)) s))
         (replace (lambda (s e) (if (== s 'pass) e s)))
         (body (: lists zipwith replace symbols es)))
    `(lambda ,args ,body))))
