%%
%% @doc HTTP client
%%
%% Callback module interface:
%%
%%      handle_headers(Status, Headers, Args) -> Result
%%          Status = {Version, Status, Comment}
%%          Headers = [{Key, Value} | ...]
%%          Key = atom() | binary()
%%          Value = binary()
%%          Args = list()
%%          Result = {ok, NewState} | {stop, Reason} | {redirect, Result}
%%
%%      handle_body(Chunk, State) -> Result
%%          Chunk = binary() | eof
%%          Result = {ok, NewState} | {stop, Reason} | ok
%%
-module(http_client).
-author("Dmitry Vasiliev <dima@hlabs.spb.ru>").
-vsn("0.2").

%% Public interface
-export([http_request/4]).

%% Behaviour information
-export([behaviour_info/1]).

-define(CONNECT_TIMEOUT, 30 * 1000).
-define(SEND_TIMEOUT, 20 * 1000).
-define(RECV_TIMEOUT, 60 * 1000).


%%
%% @doc Behaviour information
%%
behaviour_info(callbacks) ->
    [{handle_headers, 3}, {handle_body, 2}];
behaviour_info(_Other) ->
    undefined.


%%
%% @doc Request Url
%% @spec http_request(Url, Headers, Behaviour, Args) -> Result
%%      Url = string()
%%      Headers = [{Key, Value} | ...]
%%      Key = atom()
%%      Value = binary()
%%      Behaviour = atom()
%%      Args = list()
%%      Result = ok | {stop, Reason} | {error, Reason}
%%
http_request(Url, Headers, Behaviour, Args) ->
    case url:urlsplit(Url) of
        {error, Reason} ->
            {error, Reason};
        {ok, UrlParts} ->
            http_connect(UrlParts, Headers, Behaviour, Args)
    end.

http_connect({http, Host, Port, Path}, Headers, Behaviour, Args) ->
    Options = [
        {active, false},
        binary,
        inet,
        {nodelay, true},
        {packet, http_bin},
        {send_timeout, ?SEND_TIMEOUT},
        {send_timeout_close, true}
    ],
    case gen_tcp:connect(Host, Port, Options, ?CONNECT_TIMEOUT) of
        {ok, Sock} ->
            Request = http_request('GET', Path,
                http_headers(Headers, Host, [])),
            case gen_tcp:send(Sock, Request) of
                ok ->
                    Result = recv_response(Sock, Behaviour, Args),
                    gen_tcp:close(Sock),
                    Result;
                Other ->
                    Other
            end;
        Other ->
            Other
    end.


%%
%% @doc Normalize HTTP headers
%% @spec http_headers(Headers, Host, []) -> [{binary(), binary()} | ...]
%%      Headers = [{atom(), binary()} | ...]
%%      Host = binary()
%%
http_headers([{'Host', Host} | Headers], _, Collected) ->
    http_headers(Headers, seen, [{<<"Host">>, Host} | Collected]);
http_headers([{Key, Value} | Headers], Host, Collected) ->
    http_headers(Headers, Host,
        [{atom_to_binary(Key, ascii), Value} | Collected]);
http_headers([], seen, Collected) ->
    Collected;
http_headers([], Host, Collected) ->
    http_headers([], seen, [{<<"Host">>, list_to_binary(Host)} | Collected]).


%%
%% @doc Create and return HTTP request
%% @spec http_request(Method, Path, Headers) -> Request
%%      Method = 'GET'
%%      Path = binary()
%%      Headers = [{Key, Value} | ...]
%%      Key = atom()
%%      Value = binary()
%%
http_request('GET', Path, Headers) ->
    P = list_to_binary(Path),
    Header = <<"GET ",P/binary," HTTP/1.0\r\n">>,
    HeadersData = format_headers(Headers, <<>>),
    <<Header/binary,HeadersData/binary,"\r\n">>.

format_headers([], Data) ->
    Data;
format_headers([{Key, Value} | Headers], Data) ->
    format_headers(Headers,
        <<Data/binary,Key/binary,": ",Value/binary,"\r\n">>).


%%
%% @doc Receive HTTP response
%% @spec recv_response(Sock, Behaviour, Args)
%%      Sock = socket()
%%      Behaviour = atom()
%%      Args = list()
%%
recv_response(Sock, Behaviour, Args) ->
    case recv_headers(Sock, none, [], unknown) of
        {ok, Status, Headers, Size} ->
            case Behaviour:handle_headers(Status, Headers, Args) of
                {ok, State} ->
                    inet:setopts(Sock, [{packet, raw}]),
                    recv_data(Sock, Size, Behaviour, State);
                {redirect, Result} ->
                    Result;
                Other ->
                    Other
            end;
        Other ->
            Other
    end.

recv_headers(Sock, HTTPHeader, Headers, Size) ->
    case gen_tcp:recv(Sock, 0, ?RECV_TIMEOUT) of
        {ok, {http_response, Version, Status, Comment}} ->
            recv_headers(Sock, {Version, Status, Comment}, Headers, Size);
        {ok, {http_header, _, 'Content-Length', _, Value}} ->
            recv_headers(Sock, HTTPHeader,
                [{'Content-Length', Value} | Headers],
                list_to_integer(binary_to_list(Value)));
        {ok, {http_header, _, Name, _, Value}} ->
            recv_headers(Sock, HTTPHeader, [{Name, Value} | Headers], Size);
        {ok, http_eoh} ->
            {ok, HTTPHeader, lists:reverse(Headers), Size};
        {ok, {http_error, Reason}} ->
            % TODO: Return different result?
            {error, Reason};
        Other ->
            Other
    end.

recv_data(_Sock, 0, Behaviour, State) ->
    Behaviour:handle_body(eof, State);
recv_data(Sock, Size, Behaviour, State) ->
    case gen_tcp:recv(Sock, 0, ?RECV_TIMEOUT) of
        {ok, Batch} ->
            case Behaviour:handle_body(Batch, State) of
                {ok, NewState} ->
                    case Size of
                        unknown ->
                            recv_data(Sock, Size, Behaviour, NewState);
                        Num ->
                            S = Num - size(Batch),
                            recv_data(Sock, S, Behaviour, NewState)
                    end;
                Other ->
                    Other
            end;
        {error, closed} ->
            recv_data(Sock, 0, Behaviour, State);
        Other ->
            Other
    end.
