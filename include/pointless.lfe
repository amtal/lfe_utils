;; Point-free function composition. 
;;
;;  MOTIVATION:
;;
;; Functional programming is about composing pure, referentially transparent
;; functions into larger ones. The focus is on the transformations done by
;; functions, rather than state changes in data or side effects. Thus, the
;; procedural/OOP practice of naming "variables" is often pointless. They're
;; intermediate, and we know enough about them from their adjacent
;; transformations.
;;
;; Good examples are Unix pipes, Clojure's -> 'thread' macro, and point-free
;; style in Haskell via currying and the (.) ($) operators.
;;
;;  IMPLEMENTATION:
;;
;; The -> macro starts as an elegant way to write expressions like
;; f(g(h(x))) where [f..h] are functions: (-> x h g f) 
;;      
;; It is then extended to express composition of curried/specialized functions
;; like f(a,b,g(c,h(x),d),e) where [a..e] are arbitrary expressions: 
;; (-> x h (g c <> d) (f a b <> e))
;;
;; For the hell of it, it supports deep arguments so you can mix styles:
;; f(g(h(x))) can be (-> x (f (g (h <>))))
;;
;; Note the intuitive order of evaluation, a property typical f(g(h(x))) style,
;; and Haskell's f . g . h $ x style lack.
;;
;;  EXAMPLE:
;;
;; (defun get-hostname [url]
;;   (-> (case url ; strip protocol
;;         ((binary "http://" (rest bytes)) rest)
;;         ((binary "https://" (rest bytes)) rest)
;;         (x x))
;;       (binary:split <> (binary "/")) car ; strip path
;;       (binary:split <> (binary ":")) car ; strip port
;;       binary_to_list string:to_lower list_to_binary)) ; lower case
;;
;; This feels similar to pipes, or using (.) in Haskell but without the order
;; inversion. Note that any chain of expressions can be extracted into a
;; separate function. This style composes *very* well.
;;
;; The standard Erlang equivalent requires either awkwardly named intermediate
;; variables, or a pile of trivial helper functions.
;;  
;;  DETAILS:
;;
;; Since Erlang libraries are inconsistent about which side the "constant", and
;; which side the "varying" arguments go, a cut-like approach is taken. The
;; replacement is marked by <>, rather than being implicit as in Clojure.
;;
;; Single-argument functions can be used directly, no point marking their
;; argument. I'm pretty sure symbols bound in the lexical scope can also be
;; used.
;;
;; A neat trick is combining -> with cut to produce a fun, rather than an
;; expression! Trivial, but working example of a linear transformation:
;; (lists:map (cut -> <> (* a <>) (+ b <>)) vector)
(defmacro -> (es 
  (fletrec ((swap-reduce [es] (case (length es)
    (0 (exit 'empty_->_macro))
    (1 (car es)) ; fully reduced
    (_ (let* ((a (car es)) ; bottommost exp
              (b (car (cdr es))) ; target of current swap
              (rest (cdr (cdr es))) ; remaining exps
              ; Check that there's only one argument in the target exp: I
              ; don't want to complicate the semantics or deal with multiple
              ; evaluation. I'll treat 0 holes as okay for now, to simplify
              ; dealing with simple local calls.
              (count-holes 
               (fletrec ((self [e]
                         (cond 
                          ((== '<> e) 1)
                          ((is_list e) (: lists sum 
                                          (: lists map (fun self 1) e)))
                          ('true 0)))) 
                (fun self 1)))
              (assert (if (> (funcall count-holes b) 1) 
                       (error (tuple 'too_many_holes b))))
              ; perform substitution and recurse: unlike cut, this will go deep
              ; but I think that's okay as long as there's just one argument
              (merge (if (is_atom b) 
                       `(,b ,a)
                       (subst a '<> b))))
          (swap-reduce (cons merge rest)))))))
    (swap-reduce es))))
