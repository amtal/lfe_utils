;; These are unit tests. Do not mistake them for docs or references.
;;
;; See comments in include/*.lfe for those.
(include-file "include/using.lfe")
(include-file "include/gensym.lfe")
(include-file "include/cut.lfe")
(include-file "include/alambda.lfe")
(include-file "include/block.lfe")
(defmodule lfe_utils_app
  (export (start 0))
  (using gen_server math lists))

(defun start []
  (test-using)
  (test-cut)
  (test-alambda)
  (test-block)
  (: io format '"All tests passed.~n" '())
  (halt 0))

(defun test-using [] 
  (let ((_ (gen_server:module_info))
        (_ (gen_server:module_info 'exports))
        (0.0 (math:sin 0.0))
        (1.0 (math:cos 0.0))
        ('(1 2 3) (lists:sort '(2 3 1)))
        ('(1 2 3 4) (lists:append '(1 2) '(3 4))))
    'ok))

(defun test-cut []
  (let* ((a '(1 2 3))
         (b '(2 4 6))
         (b (lists:map (cut * 2 <>) a))
         (b (lists:map (cut * 2 <>) a))
         (b (lists:map (cute * (div 8 4) <>) a)))
    'ok))

(defun test-alambda []
  (let* ((fac (alambda [n] (if (== 0 n) 1 (* n (self (- n 1))))))
         ('(1 1 2 6 24 120) (lists:map fac (lists:seq 0 5))))
    'ok))

(defun test-block []
  (let* ((f (lambda [x] 
              (block sanitize
                (block checks
                  (if (< x 0) (return-from checks 'negative))
                  (if (> x 12) 
                      (if (< x 14) 
                          (return-from checks 'fishy)))
                  (if (== 7 x) (return-from sanitize 'good))
                  'unknown))))
         ('good (funcall f 7))
         ('negative (funcall f -7))
         ('fishy (funcall f 13))
         ('unknown (funcall f 42))
         (g (lambda [x] 
              (ablock proc 
                x
                (+ 1 it)
                (+ 1 it)
                (return-from proc it)
                'err)))
         (3 (funcall g 1)))
    'ok))
