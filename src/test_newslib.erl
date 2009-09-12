%%
%% @doc Library tests
%%
-module(test_newslib).
-export([test/0, generate_docs/0]).


%%
%% @doc Run all test in the library
%%
test() ->
    test([
        test_url
    ]).

test([Module | Modules]) ->
    io:format("~p: ", [Module]),
    try Module:test() of
        _ ->
            io:format("OK~n")
    catch _:Error ->
        io:format("ERROR: ~p: ~p~n", [Error, erlang:get_stacktrace()])
    end,
    test(Modules);
test([]) ->
    ok.


%%
%% @doc Generate documentation for the library
%%
generate_docs() ->
    edoc:application(newslib, "src", []).
