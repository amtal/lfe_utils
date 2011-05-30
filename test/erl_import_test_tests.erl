-module(erl_import_test_tests).
-include_lib("eunit/include/eunit.hrl").

simple_test()->
    {[_|_], [_|_]} = erl_import_app:test().
