(include-file "include/using.lfe")
(include-file "include/cut.lfe")
(defmodule lfe_utils_app
  (export (start 0))
  (using gen_server math lists))

(defun start ()
  (test-using)
  (test-cut)
  (halt 0))

(defun test-using () 
        ; familiar looking names
  (let ((_ (gen_server:module_info))
        (_ (gen_server:module_info 'exports))
        (0.0 (math:sin 0.0))
        (1.0 (math:cos 0.0))
        ('(1 2 3) (lists:sort '(2 3 1)))
        ('(1 2 3 4) (lists:append '(1 2) '(3 4))))
    (: io format 'user '"Using ok.~n" '())))

(defun test-cut ()
  (let* ((a '(1 2 3))
         (b '(2 4 6))
         (b (lists:map (cut * 2 <>) a))
         (b (lists:map (cut * 2 <>) a))
         (b (lists:map (cute * (div 8 4) <>) a)))
    (: io format 'user '"Cut ok.~n" '())))
