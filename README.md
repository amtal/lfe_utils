Including
=========

Assuming you use rebar (you should!) add the lfe_utils repository to 'deps'.
Use a specific tag, to ensure you can reconstruct the code any time later.

    {lfe_utils, ".*", {git,"git://github.com/amtal/lfe_utils.git",{tag,"v0.1.2"}}}


Features
========

Erlang-Style External Calls
---------------------------

The 'using' macro in include/using.lfe creates Erlang-style call aliases.

Pro: looks familiar.
Con: compiled module must already be in code path, so not really usable within
a single project.

Example: 

    (include-file "deps/lfe_utils/include/using.lfe")
    (defmodule my_module
      (export (test 0))
      (using lists math)) ; <--

    (defun test ()          
      (math:sin 0) 
      (lists:sort '[1 2 3])
      (lists:append '[1 2] '[3 4])
      (lists:append '[[1 2] [3 4]]))


Running the Unit Tests
======================

Run this and watch for errors: 

rebar compile -v && erl -pa ebin -s lfe_utils_app
