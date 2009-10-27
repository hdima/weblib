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
-export([start_document/2, end_document/2, start_element/4, end_element/3,
    characters/3]).

-include("simplexml.hrl").


%%
%% Behaviour callbacks
%%

start_document(_Location, []) ->
    {ok, []};
start_document(Location, {Server, N}) ->
    Server ! {start_document, Location},
    {ok, {Server, N + 1}}.

end_document(Location, {Server, N}) ->
    Server ! {end_document, Location},
    {ok, {Server, N + 1}}.

start_element(Tag, Attributes, Location, {Server, N}) ->
    Server ! {start_element, Tag, Attributes, Location},
    {ok, {Server, N + 1}}.

end_element(Tag, Location, {Server, N}) ->
    Server ! {end_element, Tag, Location},
    {ok, {Server, N + 1}}.

characters(Chunk, Location, {Server, N}) ->
    Server ! {characters, Chunk, Location},
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


flush_possible_messages() ->
    receive
        _ ->
            flush_possible_messages()
    after
        0 ->
            ok
    end.


get_trace(Chunk) ->
    flush_possible_messages(),
    Server = self(),
    {ok, {Server, N}} = simplexml:parse(Chunk, "test", ?MODULE, {Server, 0}),
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
    flush_possible_messages(),
    get_chunked_trace(Chunks, none).

get_chunked_trace([Chunk | Chunks], none) ->
    Server = self(),
    {continue, ParserState} = simplexml:parse(
        Chunk, "test", ?MODULE, {Server, 0}),
    get_chunked_trace(Chunks, {ParserState, Server});
get_chunked_trace([Chunk | Chunks], {ParserState, Server}) ->
    case simplexml:parse(Chunk, ParserState) of
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


xml_errors_test_() -> [
    ?_assertError({incomplete, #location{source="test", line=1, column=4}},
        get_chunked_trace([<<"<a>">>, eof])),
    ?_assertError({badtag, #location{source="test", line=1, column=1}},
        simplexml:parse(<<"</>">>, "test", ?MODULE, [])),
    ?_assertError({badtag, #location{source="test", line=1, column=1}},
        simplexml:parse(<<"< tag/>">>, "test", ?MODULE, [])),
    ?_assertError({badattr, #location{source="test", line=1, column=6}},
        simplexml:parse(<<"<tag =/>">>, "test", ?MODULE, [])),
    ?_assertError({badattr, #location{source="test", line=1, column=10}},
        simplexml:parse(<<"<tag name/>">>, "test", ?MODULE, [])),
    ?_assertError({badattr, #location{source="test", line=1, column=10}},
        simplexml:parse(<<"<tag name name=/>">>, "test", ?MODULE, [])),
    ?_assertError({badattr, #location{source="test", line=1, column=13}},
        simplexml:parse(<<"<tag n1='v1'n2='v2'/>">>, "test", ?MODULE, []))
    ].


simple_xml_test_() -> [
    ?_assertMatch([{start_document, _},
            {start_element, "tag", [], _},
            {end_element, "tag", _},
            {end_document, _}],
        get_trace(<<"<tag/>">>)),
    ?_assertMatch([{start_document, _},
            {start_element, "tag", [], _},
            {end_element, "tag", _},
            {end_document, _}],
        get_trace(<<"<tag />">>)),
    ?_assertMatch([{start_document, _},
            {start_element, "tag", [], _},
            {end_element, "tag", _},
            {end_document, _}],
        get_trace(<<"<tag></tag>">>)),
    ?_assertMatch([{start_document, _},
            {start_element, "tag", [], _},
            {end_element, "tag", _},
            {end_document, _}],
        get_trace(<<"<tag ></tag >">>)),
    ?_assertMatch([{start_document, _},
            {start_element, "tag", [], _},
            {characters, "Data", _},
            {end_element, "tag", _},
            {end_document, _}],
        get_trace(<<"<tag>Data</tag>">>)),
    ?_assertMatch([{start_document, _},
            {start_element, "a", [], _},
            {characters, "A", _},
            {start_element, "b", [], _},
            {characters, "B", _},
            {start_element, "c", [], _},
            {end_element, "c", _},
            {end_element, "b", _},
            {end_element, "a", _},
            {end_document, _}],
        get_trace(<<"<a>A<b>B<c/></b></a>">>))
    ].


simple_attributes_test_() -> [
    ?_assertMatch([{start_document, _},
            {start_element, "tag", [{"name", "value"}], _},
            {end_element, "tag", _},
            {end_document, _}],
        get_trace(<<"<tag name='value'/>">>)),
    ?_assertMatch([{start_document, _},
            {start_element, "tag", [{"name", " value "}], _},
            {end_element, "tag", _},
            {end_document, _}],
        get_trace(<<"<tag name = ' value ' />">>)),
    ?_assertMatch([{start_document, _},
            {start_element, "tag", [{"n1", "v1"}, {"n2", "v2"}], _},
            {end_element, "tag", _},
            {end_document, _}],
        get_trace(<<"<tag n1='v1' n2=\"v2\" />">>))
    ].


continuation_test_() -> [
    ?_assertMatch([{start_document, _},
            {start_element, "tag", [], _},
            {characters, "Da", _},
            {characters, "ta", _},
            {end_element, "tag", _},
            {end_document, _}],
        get_chunked_trace([<<"<tag>Da">>, <<"ta</tag>">>])),
    ?_assertMatch([{start_document, _},
            {start_element, "tag", [], _},
            {end_element, "tag", _},
            {end_document, _}],
        get_chunked_trace([<<"<ta">>, <<"g/>">>])),
    ?_assertMatch([{start_document, _},
            {start_element, "tag", [{"name", "value"}], _},
            {end_element, "tag", _},
            {end_document, _}],
        get_chunked_trace([<<"<">>, <<"ta">>, <<"g">>, <<" name">>, <<"='">>,
            <<"value">>, <<"'/>">>])),
    ?_assertMatch([{start_document, _},
            {start_element, "a", [], _},
            {start_element, "b", [], _},
            {end_element, "b", _},
            {start_element, "c", [], _},
            {end_element, "c", _},
            {end_element, "a", _},
            {end_document, _}],
        get_chunked_trace([<<"<a><b/><c">>, <<"/></a>">>])),
    ?_assertMatch([{start_document, _},
            {start_element, "a", [{"n", "v"}], _},
            {end_element, "a", _},
            {end_document, _}],
        get_chunked_trace([<<"<a n='v' ">>, <<" ></a>">>])),
    ?_assertMatch([{start_document, _},
            {start_element, "a", [], _},
            {end_element, "a", _},
            {end_document, _}],
        get_chunked_trace([<<"<a></a ">>, <<" >">>]))
    ].


comments_test_() -> [
    ?_assertMatch([{start_document, _},
            {start_element, "tag", [], _},
            {end_element, "tag", _},
            {end_document, _}],
        get_trace(<<"<tag><!-- Comment --></tag>">>))
    ].


processing_instructions_test_() -> [
    ?_assertMatch([{start_document, _},
            {start_element, "tag", [], _},
            {end_element, "tag", _},
            {end_document, _}],
        get_trace(<<"<tag><?target?></tag>">>))
    ].


cdata_test_() -> [
    ?_assertMatch([{start_document, _},
            {start_element, "tag", [], _},
            {characters, "<tag>Data</tag>", _},
            {end_element, "tag", _},
            {end_document, _}],
        get_trace(<<"<tag><![CDATA[<tag>Data</tag>]]></tag>">>)),
    ?_assertMatch([{start_document, _},
            {start_element, "tag", [], _},
            {characters, "&#10;&#xf;&lt;", _},
            {end_element, "tag", _},
            {end_document, _}],
        get_trace(<<"<tag><![CDATA[&#10;&#xf;&lt;]]></tag>">>))
    ].


location_test_() -> [
        ?_assertEqual([
            {start_document, #location{source="test", line=1, column=1}},
            {start_element, "a", [],
                #location{source="test", line=1, column=1}},
            {characters, " A \n ",
                #location{source="test", line=1, column=4}},
            {start_element, "b", [],
                #location{source="test", line=2, column=2}},
            {characters, " B \n ",
                #location{source="test", line=2, column=5}},
            {end_element, "b", #location{source="test", line=3, column=2}},
            {characters, " \n ",
                #location{source="test", line=3, column=6}},
            {end_element, "a", #location{source="test", line=4, column=2}},
            {end_document, #location{source="test", line=4, column=6}}
            ], get_trace(<<"<a> A \r\n <b> B \n </b> \r </a>">>)),
        ?_assertEqual([
            {start_document, #location{source="test", line=1, column=1}},
            {characters, " \n ", #location{source="test", line=2, column=5}},
            {characters, " \n ", #location{source="test", line=3, column=7}},
            {characters, "B \n ", #location{source="test", line=4, column=2}},
            {end_document, #location{source="test", line=5, column=5}}
            ], get_trace(<<"<!-- \n --> \n <?a?> \r\n <![CDATA[B \r ]]>">>))
    ].


reference_test_() -> [
    ?_assertMatch([{start_document, _},
            {start_element, "a", [], _},
            {characters, [10, 10], _},
            {end_element, "a", _},
            {end_document, _}],
        get_trace(<<"<a>&#10;&#xa;</a>">>)),
    ?_assertMatch([{start_document, _},
            {start_element, "a", [], _},
            {characters, "<>&'\"", _},
            {end_element, "a", _},
            {end_document, _}],
        get_trace(<<"<a>&lt;&gt;&amp;&apos;&quot;</a>">>)),
    ?_assertMatch([{start_document, _},
            {start_element, "a", [{"name", "<>&'\""}], _},
            {end_element, "a", _},
            {end_document, _}],
        get_trace(<<"<a name='&lt;&gt;&amp;&apos;&quot;'/>">>))
    ].


reference_errors_test_() -> [
    ?_assertError({badref, #location{source="test", line=1, column=6}},
        get_trace(<<"<a>&#a;</a>">>)),
    ?_assertError({badref, #location{source="test", line=1, column=7}},
        get_trace(<<"<a>&#xg;</a>">>)),
    ?_assertError({badref, #location{source="test", line=1, column=6}},
        get_trace(<<"<a>&#;</a>">>)),
    ?_assertError({badref, #location{source="test", line=1, column=7}},
        get_trace(<<"<a>&#x;</a>">>)),
    ?_assertError({badref, #location{source="test", line=1, column=6}},
        get_trace(<<"<a>&#</a>">>)),
    ?_assertError({badref, #location{source="test", line=1, column=7}},
        get_trace(<<"<a>&#x</a>">>))
    ].


reference_continuation_test_() -> [
    ?_assertMatch([{start_document, _},
            {start_element, "a", [], _},
            {characters, [100], _},
            {end_element, "a", _},
            {end_document, _}],
        get_chunked_trace([<<"<a>&#1">>, <<"00;</a>">>])),
    ?_assertMatch([{start_document, _},
            {start_element, "a", [], _},
            {characters, [16#fff], _},
            {end_element, "a", _},
            {end_document, _}],
        get_chunked_trace([<<"<a>&#xf">>, <<"ff;</a>">>])),
    ?_assertMatch([{start_document, _},
            {start_element, "a", [], _},
            {characters, "'", _},
            {end_element, "a", _},
            {end_document, _}],
        get_chunked_trace([<<"<a>&ap">>, <<"os;</a>">>]))
    ].
