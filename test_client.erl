-module(test_client).

-export([http_request/2]).

-export([handle_headers/4, handle_body/2]).

-behaviour(http_client).


http_request(Method, Url) ->
    http_client:http_request(Method, Url, [], ?MODULE, []).

handle_headers(Method, {_, StatNum, _}=Status, Headers, [])
        when StatNum =:= 301; StatNum =:= 302 ->
    % TODO: Limit number of redirects
    io:format("Redirect. Status: ~p~nHeaders: ~p~n~n", [Status, Headers]),
    Location = proplists:get_value('Location', Headers),
    {stop, http_request(Method, binary_to_list(Location))};
handle_headers(_Method, Status, Headers, State) ->
    io:format("Status: ~p~nHeaders: ~p~n~n", [Status, Headers]),
    {ok, State}.

handle_body(closed, _State) ->
    io:format("Closed.~n"),
    {error, closed};
handle_body(eof, _State) ->
    io:format("End.~n");
handle_body(Chunk, State) ->
    io:format("Chunk:~n~p~n~n", [Chunk]),
    {ok, State}.
