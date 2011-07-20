A set of language abstractions aimed at solving the following conundrum:

- Standard Erlang syntax was designed for imperative programmers.
- Lisp Flavored Erlang syntax was modeled after imperative Lisps.
- The Erlang VM was built for functional, not imperative, code.

Erlang is known for making some very hard things trivial, and some easy things hard. A lot of that difficulty is introduced by imperative syntax on a functional interpreter.

By implementing techniques from Clojure/Scala/Haskell as LFE macros, we can make syntax match semantics. The awkwardness of writing functional code with imperative syntax is then avoided, and previously unreadable code becomes clean and maintainable.

As techniques develop and new abstractions are found, Erlang may become useful in previously inaccessible fields.

Using
=====

Add to {deps,[...]} in rebar.config, if using rebar:

```erlang
{lfe_utils, ".*", {git,"git://github.com/amtal/lfe_utils.git",{tag,"v1.2.3"}}}
```

Add to LFE modules:

```clojure 
(include-lib "lfe_utils/include/all.lfe") 
```

Examples
========

```clojure
; Traditional (mod:fun ...) external call syntax.

(defnmodule example
  (using lists dict gen_server))

(dict:append 'foo 'bar (dict:new))

; Simple, functional alternatives to defun and lambda.

(defn ternary-or
  ['true _] 'true
  [_ 'true] 'true
  ['false 'false] 'false
  [_ _] 'undefined)

(defn hex-char?
  [c] (when (=< #x41 c) (=< c #x46)) ; A-F
      (when (=< #x61 c) (=< c #x66)) ; a-f
      (when (=< #x30 c) (=< c #x39)) ; 0-9
      'true
  [_] 'false)

(lists:foldl (fn [x acc] (+ x acc)) 0 xs)

(lists:map (fn [x] (when (< x y)) 'lt
               [x] (when (> x y)) 'gt
               [_] 'eq))


; Reversed let-binding. A natural, top-down way to write functions when you
; don't care about order of side effects. (Like when there aren't any!)

(defn list->rle [xs] 
  (in (lists:reverse encoded-xs)
      [encoded-xs (lists:foldl group seed-acc xs)
       seed-acc (tuple (car xs) 0 '())
       group (fn [x (tuple c n acc)] 
               (if (== c x) 
                 (tuple c (+ 1 n) acc)
                 (tuple x 0 (cons (cons c n) acc))))]))

; Point-free function composition and partial application. If you like Unix
; pipes you'll like this style.

(defn get-hostname [url]
  (-> (case url ; strip protocol
        ((binary "http://" (rest bytes)) rest)
        ((binary "https://" (rest bytes)) rest)
        (x x))
      (binary:split <> (binary "/")) car ; strip path
      (binary:split <> (binary ":")) car ; strip port
      binary_to_list string:to_lower list_to_binary)) ; lower case

; Function specialization for very simple funs. 
; Constructs a fun with each <> hole filled with a new argument.

(lists:filter (cut >= 0 <>) vs)
(lists:map (cut + 1 <>) xs)
(cut foo a b <> c <> d) ; specialize foo/6 to arity-2 fun

; Has an "e" (evaluated) variant, where non-hole arguments are evaluated when
; the fun's constructed, not when called. (Loop-lifting optimization.)

(defn dialyze [modules] 
  (lists:map (cute dialyze-module <> (build-plt modules))
             modules))

(defn float-re [] 
  (cute re:run <> (element 2 (re:compile '"[+-]?[0-9]*\\.?[0-9]*"))))

; Self-recursive (anaphoric) lambda variant, binding itself to 'self'.

(defn fac-fun [] (alambda [n] (if (== 0 n) 1 (* n (self (- n 1))))))

; Compile-time unique atom generation with @(gensym)@ and @(gensym prefix)@,
; for writing better macros...

; Functional-style @(block name ...)@ and @(return-to name val)@, lexically
; scoped early returns... (This is a common Erlang pattern with throw-try-catch
; blocks, and shows up every time system input is sanitized. Can't think of a
; short and simple example right now.)

; Anaphoric variant @(ablock name ...)@ where every statement in @...@ can
; access the result of the previous one as @it@. Should produce some
; interesting, procedural-esque code...

```

Consult individual files for details.

Versions
========

Many of the utilities are exploratory, or even experimental in nature. Watch the version tags: minor bumps indicate fixes and optimizations, medium bumps feature additions. Major bumps will indicate backwards incompatible changes after v1.0.0: until then, medium bumps may break compatibility.


Unit Tests
==========

Run make and watch for errors.



Note: the unique situation with general library-esque macros used via straight file inclusion, discourages the decomposition of work into many component functions. This is completely at odds with normal Erlang programming practice, and thus the code in this project is not representative of what LFE code looks like.

It is, however, lots of fun to write!

Also src/lfe_utils_app.lfe is test code, not example code. Use comments in include/*.lfe for reference instead.
