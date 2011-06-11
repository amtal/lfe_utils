
(eval-when-compile 
(defun erl-import (mod)
  (let* ((exports (try (call mod 'module_info 'exports)
                       (catch ((tuple 'error 'undef _) 
                               (exit (tuple 'module_not_found mod))))))
         (names (: lists map (fun erlang tuple_to_list 1)  exports))
         (erl-name 
           (lambda (x) (: erlang list_to_atom 
                          (: lists concat `(,mod : ,(car x))))))
         (renames (: lists map erl-name names))
         (funs (: lists map (fun erlang tuple_to_list 1) (: lists zip names renames))))
    `(rename ,mod ,@funs)))
)

(defmacro using ((list* mods)
  `(import ,@(: lists map (fun erl-import 1) mods))))
