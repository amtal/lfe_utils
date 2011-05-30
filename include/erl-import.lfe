(defmacro erl-import (mod)
  (let* ((exports (call mod 'module_info 'exports))
         (names (: lists map (fun erlang tuple_to_list 1)  exports))
         (erl-name 
           (lambda (x) (: erlang list_to_atom 
                          (: lists concat `(,mod : ,(car x))))))
         (renames (: lists map erl-name names))
         (funs (: lists map (fun erlang tuple_to_list 1) (: lists zip names renames))))
  `(rename ,mod ,@funs)))

