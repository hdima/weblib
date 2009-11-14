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

get_callbacks(List, 0) ->
    lists:sort(List);
get_callbacks(List, N) ->
    receive
        {Time, Url} ->
            get_callbacks([{round(Time), Url} | List], N - 1)
    end.


get_trace(List) ->
    get_trace(List, 0).

get_trace([], N) ->
    get_callbacks([], N);
get_trace([{Status, Url} | Tail], N) ->
    Server = self(),
    Now = get_seconds(),
    Fun = case Status of
        ok ->
            fun () -> Server ! {get_seconds() - Now, Url} end;
        fail ->
            fun () -> Server ! {get_seconds() - Now, Url}, exit(error) end
    end,
    crawler:crawl(Url, Fun),
    get_trace(Tail, N + 1).


get_seconds() ->
    {MS, S, MiS} = now(),
    MS * 1000000 + S + MiS / 1000000.


%%
%% Tests
%%

setup() ->
    crawler:start([{timeout, 1000}]).

cleanup(_) ->
    crawler:stop().


crawler_test_() -> {setup, fun setup/0, fun cleanup/1, [
    ?_assertEqual([
        {1, "http://site.com/1"},
        {2, "http://site.com/2"}
        ], get_trace([{ok, "http://site.com/1"}, {ok, "http://site.com/2"}])),
    ?_assertEqual([
        {1, "http://site1.com"},
        {1, "http://site2.com"}
        ], get_trace([{ok, "http://site1.com"}, {ok, "http://site2.com"}])),
    ?_assertEqual([
        {1, "http://site3.com"},
        {1, "http://site4.com"},
        {2, "http://site4.com"}
        ], get_trace([{ok, "http://site3.com"}, {ok, "http://site4.com"},
            {ok, "http://site4.com"}])),
    ?_assertEqual([
        {1, "http://site5.com"},
        {1, "http://site6.com"},
        {2, "http://site6.com"}
        ], get_trace([{ok, "http://site5.com"}, {fail, "http://site6.com"},
            {ok, "http://site6.com"}]))
    ]}.
