%%
%% @doc Library tests
%%
-module(test_newslib).
-export([test/0, generate_docs/0]).


%%
%% @doc Test library
%%
test() ->
    ok.


%%
%% @doc Generate documentation
%%
generate_docs() ->
    edoc:application(newslib, "src", []).
