;; Self-recursive anonymous function. Binds self as self.
;;
;; Rather trivial wrapper around fletrec, but it's in Paul Graham's
;; "On Lisp" so whatcha gonna do?
;;
;; Example: (alambda (n) (if (== 0 n) 1 (* n (self (- n 1)))))
(defmacro alambda (args body)
  `(fletrec ((self ,args ,body)) 
     (fun self ,(length args))))
