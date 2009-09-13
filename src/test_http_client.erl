%%
%% @doc Test for HTTP client
%%
-module(test_http_client).

-export([test/0]).

% Behaviour callbacks
-export([handle_headers/3, handle_body/2]).

-behaviour(http_client).


%%
%% Behaviour callbacks
%%

handle_headers(Status, Headers, [Pid]) ->
    Pid ! {handle_headers, Status, Headers},
    Pid.

handle_body(Chunk, Pid) ->
    Pid ! {handle_body, Chunk},
    Pid.


%%
%% Test methods
%%

start_test_server(Pid) ->
    {ok, Listen} = gen_tcp:listen(0,
        [binary, {ip, {127, 0, 0, 1}}, {active, false}, {packet, http_bin}]),
    {ok, Port} = inet:port(Listen),
    spawn(fun () -> start_test_client(Port, Pid) end),
    {ok, Socket} = gen_tcp:accept(Listen, 3000),
    {ok, {http_request, 'GET', {abs_path, <<"/">>}, {1, 0}}}
        = gen_tcp:recv(Socket, 0, 3000),
    {ok, {http_header, _, 'Host', _, <<"localhost">>}}
        = gen_tcp:recv(Socket, 0, 3000),
    {ok, http_eoh} = gen_tcp:recv(Socket, 0, 3000),
    ok = inet:setopts(Socket, [{packet, raw}]),
    ok = gen_tcp:send(Socket,
        <<"HTTP/1.0 200 OK\r\nContent-Length: 2\r\n\r\nOK">>),
    ok = gen_tcp:close(Socket),
    ok = gen_tcp:close(Listen).


start_test_client(Port, Pid) ->
    http_client:http_request("http://localhost:" ++ integer_to_list(Port),
        [], test_http_client, [Pid]).


test_http_client([3, 2, 1]) ->
    ok;
test_http_client(Result) ->
    receive
        {handle_headers, {{1, 0}, 200, <<"OK">>},
                [{'Content-Length', <<"2">>}]} ->
            test_http_client([1 | Result]);
        {handle_body, <<"OK">>} ->
            test_http_client([2 | Result]);
        {handle_body, eof} ->
            test_http_client([3 | Result])
    after
        3000 ->
            error
    end.


test() ->
    Pid = self(),
    spawn(fun () -> start_test_server(Pid) end),
    ok = test_http_client([]),
    ok.
