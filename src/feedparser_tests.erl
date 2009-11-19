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
%% @doc Tests for feed parser
%%
-module(feedparser_tests).
-author("Dmitry Vasiliev <dima@hlabs.spb.ru>").
-vsn("0.1").

-include_lib("eunit/include/eunit.hrl").

-behaviour(feedparser).

%% Behaviour callbacks
-export([start_channel/1, end_channel/1, channel_info/2, news_item/2]).

-include("feedparser.hrl").


%%
%% Behaviour callbacks
%%

start_channel({Server, N}) ->
    Server ! start_channel,
    {ok, {Server, N + 1}}.

end_channel({Server, N}) ->
    Server ! end_channel,
    {ok, {Server, N + 1}}.

channel_info(ChannelInfo, {Server, N}) ->
    Server ! {channel_info, ChannelInfo},
    {ok, {Server, N + 1}}.

news_item(NewsInfo, {Server, N}) ->
    Server ! {news_item, NewsInfo},
    {ok, {Server, N + 1}}.


%%
%% Auxiliary functions
%%


flush_possible_messages() ->
    receive
        _ ->
            flush_possible_messages()
    after
        0 ->
            ok
    end.


get_callbacks(N) ->
    self() ! eof,
    get_callbacks([], N).

get_callbacks(List, N) ->
    receive
        eof when N =:= 0 ->
            lists:reverse(List);
        eof ->
            error;
        Info ->
            get_callbacks([Info | List], N - 1)
    end.


get_trace([Chunk | Chunks]) ->
    flush_possible_messages(),
    Server = self(),
    case feedparser:parse(Chunk, "test", ?MODULE, {Server, 0}) of
        {continue, ParserState} ->
            get_trace(Chunks, {ParserState, Server});
        {ok, {Server, N}} when Chunks =:= [] ->
            get_callbacks(N)
    end.

get_trace([Chunk | Chunks], {ParserState, Server}) ->
    case feedparser:parse(Chunk, ParserState) of
        {continue, NewParserState} ->
            get_trace(Chunks, {NewParserState, Server});
        {ok, {Server, N}} when Chunks =:= [] ->
            get_callbacks(N)
    end.

%%
%% Tests
%%

setup() ->
    encodings:start().

cleanup(_) ->
    encodings:stop().


rss_test_() -> {setup, fun setup/0, fun cleanup/1, [
    ?_assertMatch([start_channel,
            {channel_info, #channel_info{
                title="Title",
                link="http://feed.com",
                id="http://feed.com",
                description="Description",
                language="en",
                copyright="Copyright"}},
            end_channel],
        get_trace([<<"<rss><channel>">>,
            <<"<title>Title</title>">>,
            <<"<link>http://feed.com</link>">>,
            <<"<description>Descr">>, <<"iption</description>">>,
            <<"<language>en</language>">>,
            <<"<copyright>Copyright</copyright>">>,
            <<"</channel></rss>">>]))
    ]}.
