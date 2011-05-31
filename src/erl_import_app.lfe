;; Unit test for include/erl_import.lfe
(include-file "include/erl_import.lfe")
(defmodule erl_import_app
  (export (start 0))
  (import (rename math ((min 2) erl:min)))
  (using gen_server math)
  (import (rename math ((max 2) erl:max))))

(defun start () 
        ; familiar looking names
  (let ((_ (gen_server:module_info))
        (_ (gen_server:module_info 'exports))
        ; does not interfere with existing renames
        (0.0 (math:sin 0))
        (1 (min 1 2))
        (2 (max 1 2)))
    (: io format 'user '"Test ok.~n" '())
    'test_ok))

