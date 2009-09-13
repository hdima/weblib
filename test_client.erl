-module(test_client).

-export([http_request/1]).

-export([handle_headers/3, handle_body/2]).

-behaviour(http_client).


http_request(Url) ->
    http_client:http_request(Url, [], ?MODULE, []).

handle_headers({_, StatNum, _}=Status, Headers, _State)
        when StatNum =:= 301; StatNum =:= 302 ->
    % TODO: Limit number of redirects
    io:format("Redirect. Status: ~p~nHeaders: ~p~n~n", [Status, Headers]),
    Location = proplists:get_value('Location', Headers),
    {stop, http_request(binary_to_list(Location))};
handle_headers(Status, Headers, State) ->
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
