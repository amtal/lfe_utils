;; Early exits.
;;
;; Sort of lexically scoped: you could get around it, if you tried to on
;; purpose.
(defmacro block ((cons name body)
  (let* ((id (list_to_atom (: lists concat (list 'block- name))))
         (body (change-return-froms-to-throws name id body)))
    `(try (progn ,@body) 
          (catch ((tuple 'throw (tuple ',id val) _) 
                  val))))))

;; Quick implementation of Paul Graham's anaphoric variant
;; from On Lisp. Should allow a very procedural code style.
;;
;; Binds the result of each expression in the body as 'it' for
;; the next instruction.
(defmacro ablock ((cons name body)
  (let* ((id (list_to_atom (: lists concat (list 'ablock- name))))
         (body (change-return-froms-to-throws name id body))
         (body (: lists map (lambda [x] `(it ,x)) body)))
    `(try (let* ,body it) 
          (catch ((tuple 'throw (tuple ',id val) _) 
                  val))))))

(eval-when-compile
(defun change-return-froms-to-throws 
  ; replace return-from/2 with throw
 ([name id (list 'return-from name* val)] 
   (when (== name name*))
   `(throw (tuple ',id ,val)))
  ; check for nested identically named blocks (weird)
  ([name _ (cons 'block (cons name* _))] 
   (when (== name name*))
   (error (tuple 'repeat_block_name name*)))
  ; recurse down tree otherwise
  ([name id es] (when (is_list es)) 
   (lc ((<- e es)) 
    (change-return-froms-to-throws name id e)))
  ; until leaves are hit
  ([_ _ x] x))
)
