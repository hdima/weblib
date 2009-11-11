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
%% @doc Test for crawler
%%
-module(crawler_tests).
-author("Dmitry Vasiliev <dima@hlabs.spb.ru>").
-vsn("0.1").

-include_lib("eunit/include/eunit.hrl").


%%
%% Auxiliary functions
%%

-spec start_test_server(atom(), list(), term(), binary()) -> no_return().

start_test_server(Method, Headers, State, Response) ->
    {ok, Listen} = gen_tcp:listen(0,
        [binary, {ip, {127, 0, 0, 1}}, {active, false}, {packet, http_bin}]),
    {ok, Port} = inet:port(Listen),
    spawn(fun () -> start_test_client(Method, Port, Headers, State) end),
    {ok, Socket} = gen_tcp:accept(Listen, 3000),
    {ok, {http_request, Method, {abs_path, <<"/">>}, {1, 0}}}
        = gen_tcp:recv(Socket, 0, 3000),
    {ok, {http_header, _, 'Host', _, <<"localhost:",_/binary>>}}
        = gen_tcp:recv(Socket, 0, 3000),
    check_headers(Socket, Headers),
    ok = inet:setopts(Socket, [{packet, raw}]),
    ok = gen_tcp:send(Socket, Response),
    ok = gen_tcp:close(Socket),
    ok = gen_tcp:close(Listen).


check_headers(Socket, []) ->
    {ok, http_eoh} = gen_tcp:recv(Socket, 0, 3000);
check_headers(Socket, [{Key, Value} | Headers]) ->
    K = atom_to_binary(Key, latin1),
    {ok, {http_header, _, K, _, Value}} = gen_tcp:recv(Socket, 0, 3000),
    check_headers(Socket, Headers).


start_test_client(Method, Port, Headers, {Server, _}=State) ->
    http_client:http_request(Method,
        "http://localhost:" ++ integer_to_list(Port), Headers, ?MODULE, State),
    Server ! eof.


get_trace(Method, Headers, State, Response) ->
    Pid = self(),
    spawn(fun () -> start_test_server(Method, Headers,
        {Pid, State}, Response) end),
    get_callbacks([]).


get_callbacks(List) ->
    receive
        eof ->
            lists:reverse(List);
        Info ->
            get_callbacks([Info | List])
    after
        3000 ->
            error
    end.


%%
%% Tests
%%
