;; Generate a unique symbol for use within hygeinic macros.
;;
;; This avoids symbol collision (and ensuing heisenbugs due to shadowing)
;; which complex code expanding macros may introduce. The symbol is unique to
;; the process that evals this call during compilation, which, while
;; implementation specific, is probably good enough.
;;
;; (gensym) -> 'gensym-0, 'gensym-1, ...
;; (gensym foo) -> 'foo-0, 'foo-1, ...
;;
;; And that's right. I'm using the -compiler's- process dictionary.
;;
;; What up? B)
(eval-when-compile
(defun gensym () (gensym 'gensym))
(defun gensym (prefix)
  (let* ((pdict-key (tuple 'gensym prefix))
         (next-index (case (get pdict-key) 
                       ('undefined 0)
                       (n (+ 1 n))))
         (sym-name (: lists concat `(,prefix "-" ,next-index))))
    (put pdict-key next-index)
    (list_to_atom sym-name)))
)
;; An alternate implementation would use to_existing_atom's exceptions to find
;; a unique id. Iterating from 0..N each time struck me as bad, though, and
;; then I realized it would have a non-unique race condition during parallel
;; compilation anyway. Which led me to this implementation, since
;; per-compile-process uniqueness should be sufficient to guarantee no
;; shadowing.
;;
;; Would this best be part of the compiler?
