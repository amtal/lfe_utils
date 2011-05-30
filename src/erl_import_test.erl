-include_lib("eunit/include/eunit.hrl").
-module(erl_import_test).
-export([test/0]).

test() ->
    {_,_} = erl_import_app:test().
