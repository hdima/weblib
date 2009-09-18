%% Copyright (c) 2009, Dmitry Vasiliev <dima@hlabs.spb.ru>
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%
%% * Redistributions of source code must retain the above copyright notice,
%%   this list of conditions and the following disclaimer.
%% * Redistributions in binary form must reproduce the above copyright notice,
%%   this list of conditions and the following disclaimer in the documentation
%%   and/or other materials provided with the distribution.
%% * Neither the name of the copyright holders nor the names of its
%%   contributors may be used to endorse or promote products derived from this
%%   software without specific prior written permission. 
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.
%%
%% @doc HTTP client
%%
%% Callback module interface:
%%
%%      handle_headers(Method, Status, Headers, Args) -> Result
%%          Status = {Version, Status, Comment}
%%          Version = {Major=int(), Minor=int()}
%%          Headers = [{Key, Value} | ...]
%%          Key = atom() | binary()
%%          Value = binary()
%%          Args = term()
%%          Result = {ok, State} | {stop, State}
%%
%%      handle_body(Chunk, State) -> Result
%%          Chunk = binary() | eof | closed
%%          Result = {ok, NewState} | {stop, State}
%%
-module(http_client).
-author("Dmitry Vasiliev <dima@hlabs.spb.ru>").
-vsn("0.4").

%% Public interface
-export([http_request/5]).

%% Behaviour information
-export([behaviour_info/1]).

-define(CONNECT_TIMEOUT, 30 * 1000).
-define(SEND_TIMEOUT, 20 * 1000).
-define(RECV_TIMEOUT, 60 * 1000).


%%
%% @doc Behaviour information
%%
behaviour_info(callbacks) ->
    [{handle_headers, 4}, {handle_body, 2}];
behaviour_info(_Other) ->
    undefined.


%%
%% @doc Send HTTP request
%% @spec http_request(Method, Url, Headers, Behaviour, Args) -> ok
%%      Method = 'GET' | 'HEAD'
%%      Url = string()
%%      Headers = [{Key, Value} | ...]
%%      Key = atom()
%%      Value = binary()
%%      Behaviour = atom()
%%      Args = term()
%%
http_request(Method, Url, Headers, Behaviour, Args) ->
    http_connect(url:urlsplit(Url), Headers, Method, Behaviour, Args),
    ok.


http_connect({http, Host, Port, _}=Url, Headers, Method, Behaviour, Args) ->
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
            try send_request(Sock, Url, Headers, Method, Behaviour, Args)
            after
                gen_tcp:close(Sock)
            end;
        {error, Reason} ->
            erlang:error(http_connect_error, [Reason])
    end.


send_request(Sock, {http, Host, Port, Path},
        Headers, Method, Behaviour, Args) ->
    Request = create_request(Method, Path,
        http_headers(Headers, {http, Host, Port}, [])),
    case gen_tcp:send(Sock, Request) of
        ok ->
            recv_response(Sock, Method, Behaviour, Args);
        {error, Reason} ->
            erlang:error(http_send_error, [Reason])
    end.


%%
%% @doc Normalize HTTP headers
%% @spec http_headers(Headers, Host, []) -> [{binary(), binary()} | ...]
%%      Headers = [{atom(), binary()} | ...]
%%      Host = {http, list(), integer()}
%%
http_headers([{'Host', Host} | Headers], _, Collected) ->
    http_headers(Headers, seen, [{<<"Host">>, Host} | Collected]);
http_headers([{Key, Value} | Headers], Host, Collected) ->
    http_headers(Headers, Host,
        [{atom_to_binary(Key, ascii), Value} | Collected]);
http_headers([], seen, Collected) ->
    Collected;
http_headers([], {http, Host, Port}, Collected) ->
    H = list_to_binary(Host),
    case Port of
        80 ->
            % Default HTTP port
            Val = H;
        Port ->
            P = list_to_binary(integer_to_list(Port)),
            Val = <<H/binary,":",P/binary>>
    end,
    http_headers([], seen, [{<<"Host">>, Val} | Collected]).


%%
%% @doc Create and return HTTP request
%% @spec create_request(Method, Path, Headers) -> Request
%%      Method = 'GET'
%%      Path = binary()
%%      Headers = [{Key, Value} | ...]
%%      Key = atom()
%%      Value = binary()
%%
create_request(Method, Path, Headers)
        when Method =:= 'GET'; Method =:= 'HEAD' ->
    M = atom_to_binary(Method, latin1),
    P = list_to_binary(Path),
    Status = <<M/binary," ",P/binary," HTTP/1.0\r\n">>,
    HeadersData = format_headers(Headers, <<>>),
    <<Status/binary,HeadersData/binary,"\r\n">>.

format_headers([], Data) ->
    Data;
format_headers([{Key, Value} | Headers], Data) ->
    format_headers(Headers,
        <<Data/binary,Key/binary,": ",Value/binary,"\r\n">>).


%%
%% @doc Receive HTTP response
%% @spec recv_response(Sock, Method, Behaviour, Args)
%%      Sock = socket()
%%      Method = atom()
%%      Behaviour = atom()
%%      Args = term()
%%
recv_response(Sock, Method, Behaviour, Args) ->
    {Status, Headers, Size} = recv_headers(Sock, none, [], unknown), 
    case Behaviour:handle_headers(Method, Status, Headers, Args) of
        {ok, State} ->
            {_, S, _} = Status,
            case Method of
                'HEAD' ->
                    % Ignore body per RFC2616
                    Behaviour:handle_body(eof, State);
                _ when S < 200; S =:= 204; S =:= 304 ->
                    % Ignore body per RFC2616
                    Behaviour:handle_body(eof, State);
                _ ->
                    inet:setopts(Sock, [{packet, raw}]),
                    recv_data(Sock, Size, Behaviour, State)
            end;
        {stop, State} ->
            Behaviour:handle_body(eof, State)
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
            {HTTPHeader, lists:reverse(Headers), Size};
        {ok, {http_error, Reason}} ->
            erlang:error(http_error, [Reason]);
        {error, Reason} ->
            erlang:error(http_receive_error, [Reason])
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
                            recv_data(Sock, unknown, Behaviour, NewState);
                        Num ->
                            S = Num - size(Batch),
                            recv_data(Sock, S, Behaviour, NewState)
                    end;
                {stop, State} ->
                    Behaviour:handle_body(eof, State)
            end;
        {error, closed} ->
            case Size of
                unknown ->
                    Behaviour:handle_body(eof, State);
                _ ->
                    Behaviour:handle_body(closed, State),
                    erlang:error(http_receive_error, [closed])
            end;
        {error, Reason} ->
            Behaviour:handle_body(closed, State),
            erlang:error(http_receive_error, [Reason])
    end.
