;; Unit test for include/using.lfe
(include-file "include/using.lfe")
(defmodule lfe_utils_app
  (export (start 0))
  (using gen_server math lists))

(defun start () 
        ; familiar looking names
  (let ((_ (gen_server:module_info))
        (_ (gen_server:module_info 'exports))
        (0.0 (math:sin 0.0))
        (1.0 (math:cos 0.0))
        ('(1 2 3) (lists:sort '(2 3 1)))
        ('(1 2 3 4) (lists:append '(1 2) '(3 4))))
    (: io format 'user '"Tests ok.~n" '())
    (halt 0)
    'ok))

