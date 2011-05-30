(include-file "include/erl-import.lfe")
(defmodule erl_import_app
  (export (test 0))
  (import (erl-import gen_server)))

(defun test () 
  (tuple (gen_server:module_info) 
         (gen_server:module_info 'exports)))

