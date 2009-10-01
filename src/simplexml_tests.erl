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
-module(simplexml_tests).

-include_lib("eunit/include/eunit.hrl").

-behaviour(simplexml).

%% Behaviour callbacks
-export([start_document/1, end_document/1, start_element/3, end_element/2,
    characters/2]).

-include("simplexml.hrl").


%%
%% Behaviour callbacks
%%

start_document([]) ->
    {ok, []};
start_document({Server, N}) ->
    Server ! start_document,
    {ok, {Server, N + 1}}.

end_document({Server, N}) ->
    Server ! end_document,
    {ok, {Server, N + 1}}.

start_element(Tag, Attributes, {Server, N}) ->
    Server ! {start_element, Tag, Attributes},
    {ok, {Server, N + 1}}.

end_element(Tag, {Server, N}) ->
    Server ! {end_element, Tag},
    {ok, {Server, N + 1}}.

characters(Chunk, {Server, N}) ->
    Server ! {characters, Chunk},
    {ok, {Server, N + 1}}.


%%
%% Auxiliary functions
%%

get_char_type(C) when ?is_whitespace(C) ->
    whitespace;
get_char_type(C) when ?is_namestartchar(C) ->
    namestartchar;
get_char_type(C) when ?is_namechar(C) ->
    namechar;
get_char_type(C) when ?is_attrvaluechar(C, "'") ->
    attrvaluechar;
get_char_type(_) ->
    other.


get_trace(Chunk) ->
    Server = self(),
    {ok, {Server, N}} = simplexml:parse(Chunk, ?MODULE, {Server, 0}),
    get_callbacks(N).


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
    after
        500 ->
            error
    end.


get_chunked_trace(Chunks) ->
    get_chunked_trace(Chunks, none).

get_chunked_trace([Chunk | Chunks], none) ->
    Server = self(),
    {continue, DocId} = simplexml:parse(Chunk, ?MODULE, {Server, 0}),
    get_chunked_trace(Chunks, {DocId, Server});
get_chunked_trace([Chunk | Chunks], {DocId, Server}) ->
    case simplexml:parse(Chunk, DocId) of
        {continue, NewId} ->
            get_chunked_trace(Chunks, {NewId, Server});
        {ok, {Server, N}} ->
            get_callbacks(N)
    end.

%%
%% Tests
%%

constants_test_() -> [
    ?_assertEqual(whitespace, get_char_type(16#20)),
    ?_assertEqual(whitespace, get_char_type(16#9)),
    ?_assertEqual(whitespace, get_char_type(16#D)),
    ?_assertEqual(whitespace, get_char_type(16#A)),
    ?_assertEqual(namestartchar, get_char_type($:)),
    ?_assertEqual(namestartchar, get_char_type($_)),
    ?_assertEqual(namestartchar, get_char_type($a)),
    ?_assertEqual(namestartchar, get_char_type($z)),
    ?_assertEqual(namestartchar, get_char_type($A)),
    ?_assertEqual(namestartchar, get_char_type($Z)),
    ?_assertEqual(namechar, get_char_type($-)),
    ?_assertEqual(namechar, get_char_type($.)),
    ?_assertEqual(namechar, get_char_type($0)),
    ?_assertEqual(namechar, get_char_type($9)),
    ?_assertEqual(attrvaluechar, get_char_type($?))
    ].


errors_test_() -> [
    ?_assertError(xml_badtag, simplexml:parse(<<"</>">>, ?MODULE, [])),
    ?_assertError(xml_badtag, simplexml:parse(<<"< tag/>">>, ?MODULE, [])),
    ?_assertError(xml_badattr, simplexml:parse(<<"<tag =/>">>, ?MODULE, [])),
    ?_assertError(xml_badattr, simplexml:parse(<<"<tag name/>">>, ?MODULE, [])),
    ?_assertError(xml_badattr,
        simplexml:parse(<<"<tag name name=/>">>, ?MODULE, [])),
    ?_assertError(xml_badattr,
        simplexml:parse(<<"<tag n1='v1'n2='v2'/>">>, ?MODULE, []))
    ].


simple_xml_test_() -> [
    ?_assertEqual([start_document,
            {start_element, "tag", []},
            {end_element, "tag"},
            end_document],
        get_trace(<<"<tag/>">>)),
    ?_assertEqual([start_document,
            {start_element, "tag", []},
            {end_element, "tag"},
            end_document],
        get_trace(<<"<tag />">>)),
    ?_assertEqual([start_document,
            {start_element, "tag", []},
            {end_element, "tag"},
            end_document],
        get_trace(<<"<tag></tag>">>)),
    ?_assertEqual([start_document,
            {start_element, "tag", []},
            {end_element, "tag"},
            end_document],
        get_trace(<<"<tag ></tag >">>)),
    ?_assertEqual([start_document,
            {start_element, "tag", []},
            {characters, "Data"},
            {end_element, "tag"},
            end_document],
        get_trace(<<"<tag>Data</tag>">>)),
    ?_assertEqual([start_document,
            {start_element, "a", []},
            {characters, "A"},
            {start_element, "b", []},
            {characters, "B"},
            {start_element, "c", []},
            {end_element, "c"},
            {end_element, "b"},
            {end_element, "a"},
            end_document],
        get_trace(<<"<a>A<b>B<c/></b></a>">>))
    ].


simple_attributes_test_() -> [
    ?_assertEqual([start_document,
            {start_element, "tag", [{"name", "value"}]},
            {end_element, "tag"},
            end_document],
        get_trace(<<"<tag name='value'/>">>)),
    ?_assertEqual([start_document,
            {start_element, "tag", [{"name", " value "}]},
            {end_element, "tag"},
            end_document],
        get_trace(<<"<tag name = ' value ' />">>)),
    ?_assertEqual([start_document,
            {start_element, "tag", [{"n1", "v1"}, {"n2", "v2"}]},
            {end_element, "tag"},
            end_document],
        get_trace(<<"<tag n1='v1' n2=\"v2\" />">>))
    ].


continuation_test_() -> [
    ?_assertEqual([start_document,
            {start_element, "tag", []},
            {characters, "Da"},
            {characters, "ta"},
            {end_element, "tag"},
            end_document],
        get_chunked_trace([<<"<tag>Da">>, <<"ta</tag>">>])),
    ?_assertEqual([start_document,
            {start_element, "tag", []},
            {end_element, "tag"},
            end_document],
        get_chunked_trace([<<"<ta">>, <<"g/>">>])),
    ?_assertEqual([start_document,
            {start_element, "tag", [{"name", "value"}]},
            {end_element, "tag"},
            end_document],
        get_chunked_trace([<<"<">>, <<"ta">>, <<"g">>, <<" name">>, <<"='">>,
            <<"value">>, <<"'/>">>])),
    ?_assertEqual([start_document,
            {start_element, "a", []},
            {start_element, "b", []},
            {end_element, "b"},
            {start_element, "c", []},
            {end_element, "c"},
            {end_element, "a"},
            end_document],
        get_chunked_trace([<<"<a><b/><c">>, <<"/></a>">>])),
    ?_assertEqual([start_document,
            {start_element, "a", [{"n", "v"}]},
            {end_element, "a"},
            end_document],
        get_chunked_trace([<<"<a n='v' ">>, <<" ></a>">>])),
    ?_assertEqual([start_document,
            {start_element, "a", []},
            {end_element, "a"},
            end_document],
        get_chunked_trace([<<"<a></a ">>, <<" >">>]))
    ].


comments_test_() -> [
    ?_assertEqual([start_document,
            {start_element, "tag", []},
            {end_element, "tag"},
            end_document],
        get_trace(<<"<tag><!-- Comment --></tag>">>))
    ].


processing_instructions_test_() -> [
    ?_assertEqual([start_document,
            {start_element, "tag", []},
            {end_element, "tag"},
            end_document],
        get_trace(<<"<tag><?target?></tag>">>))
    ].


cdata_test_() -> [
    ?_assertEqual([start_document,
            {start_element, "tag", []},
            {characters, "<tag>Data</tag>"},
            {end_element, "tag"},
            end_document],
        get_trace(<<"<tag><![CDATA[<tag>Data</tag>]]></tag>">>))
    ].
