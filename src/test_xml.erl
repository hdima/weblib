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
%% @doc Tests for relaxed XML module
%%
-module(test_xml).

-export([test/0]).

-behaviour(xml).

%% Behaviour callbacks
-export([start_document/1, end_document/1, start_element/3, end_element/2,
    characters/2]).


%%
%% Behaviour callbacks
%%

start_document(Args) ->
    {ok, [{start_document} | Args]}.

end_document(State) ->
    {ok, [{end_document} | State]}.

start_element(Tag, Attributes, State) ->
    {ok, [{start_element, Tag, Attributes} | State]}.

end_element(Tag, State) ->
    {ok, [{end_element, Tag} | State]}.

characters(Chunk, State) ->
    {ok, [{characters, Chunk} | State]}.


%%
%% Tests
%%

get_trace(Chunk, Behaviour, Args) ->
    {ok, Trace} = xml:parse(Chunk, Behaviour, Args),
    lists:reverse(Trace).


test_errors() ->
    {'EXIT', {xml_nodata, _}} = (catch xml:parse(<<>>, ?MODULE, [])),
    {'EXIT', {xml_badtag, _}} = (catch xml:parse(<<"< tag/>">>, ?MODULE, [])),
    ok.


test_simple_xml() ->
    [{start_document},
        {start_element, "tag", []},
        {end_element, "tag"},
        {end_document}] = get_trace(<<"<tag/>">>, ?MODULE, []),
    [{start_document},
        {start_element, "tag", []},
        {end_element, "tag"},
        {end_document}] = get_trace(<<"<tag />">>, ?MODULE, []),
    ok.


test() ->
    test_errors(),
    test_simple_xml(),
    ok.
