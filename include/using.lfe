
(eval-when-compile 
;; Constructs a (rename mod ...) expression based on mod:module_info(exports) 
(defun generate-erl-call-renames (mod)
  (let* ((exports ; get list of exports, give useful error if module DNE
           (try (call mod 'module_info 'exports)
                (catch ((tuple 'error 'undef _) 
                        (exit (tuple 'module_not_found mod))))))
         (exports ; list of (fun-name fun-arity)
           (: lists map (fun tuple_to_list 1) 
                        exports))
         (make-erl-name
           (lambda (x) 
             (list_to_atom (: lists concat `(,mod : ,(car x))))))
         (erlnames ; list of 'mod:fun 
           (: lists map make-erl-name 
                        exports))
         (renames ; list of ((fun arity) newname)
           (: lists map (fun tuple_to_list 1) 
                        (: lists zip exports 
                                     erlnames))))
    `(rename ,mod ,@renames)))
)

(defmacro using ((list* mods)
  `(import ,@(: lists map (fun generate-erl-call-renames 1) 
                          mods))))
