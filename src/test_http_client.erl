%%
%% @doc Test for HTTP client
%%
-module(test_http_client).

-export([test/0]).

% Behaviour callbacks
-export([handle_headers/4, handle_body/2]).

-behaviour(http_client).


%%
%% Behaviour callbacks
%%

handle_headers(Method, Status, Headers, {Pid, Continue}) ->
    Pid ! {handle_headers, Method, Status, Headers},
    case Continue of
        ok ->
            {ok, Pid};
        stop ->
            {stop, Pid}
    end.

handle_body(closed, Pid) ->
    Pid ! {handle_body, closed},
    {error, closed};
handle_body(eof, Pid) ->
    Pid ! {handle_body, eof},
    ok;
handle_body(Chunk, Pid) ->
    Pid ! {handle_body, Chunk},
    {ok, Pid}.


%%
%% @doc Start test server
%%
start_test_server(Method, State, Response) ->
    {ok, Listen} = gen_tcp:listen(0,
        [binary, {ip, {127, 0, 0, 1}}, {active, false}, {packet, http_bin}]),
    {ok, Port} = inet:port(Listen),
    spawn(fun () -> start_test_client(Method, Port, State) end),
    {ok, Socket} = gen_tcp:accept(Listen, 3000),
    {ok, {http_request, Method, {abs_path, <<"/">>}, {1, 0}}}
        = gen_tcp:recv(Socket, 0, 3000),
    {ok, {http_header, _, 'Host', _, <<"localhost:",_/binary>>}}
        = gen_tcp:recv(Socket, 0, 3000),
    {ok, http_eoh} = gen_tcp:recv(Socket, 0, 3000),
    ok = inet:setopts(Socket, [{packet, raw}]),
    ok = gen_tcp:send(Socket, Response),
    ok = gen_tcp:close(Socket),
    ok = gen_tcp:close(Listen).


%%
%% @doc Start test client
%%
start_test_client(Method, Port, State) ->
    http_client:http_request(Method,
        "http://localhost:" ++ integer_to_list(Port),
        [], test_http_client, State).


test_http_client([]) ->
    ok;
test_http_client([Pattern | Results]) ->
    receive
        Pattern ->
            test_http_client(Results)
    after
        3000 ->
            error
    end.


test_get_request() ->
    Pid = self(),
    Response = <<"HTTP/1.0 200 OK\r\nContent-Length: 2\r\n\r\nOK">>,
    spawn(fun () -> start_test_server('GET', {Pid, ok}, Response) end),
    ok = test_http_client([
        {handle_headers, 'GET', {{1, 0}, 200, <<"OK">>},
            [{'Content-Length', <<"2">>}]},
        {handle_body, <<"OK">>},
        {handle_body, eof}
    ]).


test_stop_response() ->
    Pid = self(),
    Response = <<"HTTP/1.0 200 OK\r\n\r\nOK">>,
    spawn(fun () -> start_test_server('GET', {Pid, stop}, Response) end),
    ok = test_http_client([
        {handle_headers, 'GET', {{1, 0}, 200, <<"OK">>}, []},
        {handle_body, eof}
    ]).


test_responses_without_content_length() ->
    Pid = self(),
    Response = <<"HTTP/1.0 200 OK\r\n\r\nOK">>,
    spawn(fun () -> start_test_server('GET', {Pid, ok}, Response) end),
    ok = test_http_client([
        {handle_headers, 'GET', {{1, 0}, 200, <<"OK">>}, []},
        {handle_body, <<"OK">>},
        {handle_body, eof}
    ]).

test_head_request() ->
    Pid = self(),
    Response = <<"HTTP/1.0 200 OK\r\n\r\nOK">>,
    spawn(fun () -> start_test_server('HEAD', {Pid, ok}, Response) end),
    ok = test_http_client([
        {handle_headers, 'HEAD', {{1, 0}, 200, <<"OK">>}, []},
        {handle_body, eof}
    ]).


test() ->
    test_get_request(),
    test_stop_response(),
    test_responses_without_content_length(),
    test_head_request(),
    ok.
