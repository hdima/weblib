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
-author("Dmitry Vasiliev <dima@hlabs.spb.ru>").
-vsn("0.1").

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


get_chars([<<>>]) ->
    [];
get_chars([<<>>, <<C, Tail/binary>> | Chunks]) ->
    [<<C>> | get_chars([Tail | Chunks])];
get_chars([<<C, Tail/binary>> | Chunks]) ->
    [<<C>> | get_chars([Tail | Chunks])].


get_trace([Chunk | Chunks]) ->
    flush_possible_messages(),
    Server = self(),
    case simplexml:parse(Chunk, "test", ?MODULE, {Server, 0}) of
        {continue, ParserState} ->
            get_trace(Chunks, {ParserState, Server});
        {ok, {Server, N}} when Chunks =:= [] ->
            get_callbacks(N)
    end.

get_trace([Chunk | Chunks], {ParserState, Server}) ->
    case simplexml:parse(Chunk, ParserState) of
        {continue, NewParserState} ->
            get_trace(Chunks, {NewParserState, Server});
        {ok, {Server, N}} when Chunks =:= [] ->
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


setup() ->
    encodings:start().

cleanup(_) ->
    encodings:stop().


xml_errors_test_() -> {setup, fun setup/0, fun cleanup/1, [
    ?_assertError({incomplete, #location{source="test", line=1, column=4}},
        get_trace([<<"<a>">>, eof])),
    ?_assertError({badtag, #location{source="test", line=1, column=4}},
        get_trace([<<"<a></b>">>])),
    ?_assertError({badtag, #location{source="test", line=1, column=8}},
        get_trace([<<"<a></a><b></b>">>])),
    ?_assertError({badtag, #location{source="test", line=1, column=1}},
        get_trace([<<"</>">>])),
    ?_assertError({badtag, #location{source="test", line=1, column=1}},
        get_trace([<<"< tag/>">>])),
    ?_assertError({badattr, #location{source="test", line=1, column=6}},
        get_trace([<<"<tag =/>">>])),
    ?_assertError({badattr, #location{source="test", line=1, column=10}},
        get_trace([<<"<tag name/>">>])),
    ?_assertError({badattr, #location{source="test", line=1, column=10}},
        get_trace([<<"<tag name name=/>">>])),
    ?_assertError({badattr, #location{source="test", line=1, column=13}},
        get_trace([<<"<tag n1='v1'n2='v2'/>">>]))
    ]}.


simple_xml_test_() -> {setup, fun setup/0, fun cleanup/1, [
    ?_assertMatch([{start_document, _},
            {start_element, {"", "tag", "tag"}, [], _},
            {end_element, {"", "tag", "tag"}, _},
            {end_document, _}],
        get_trace([<<"<tag/>">>])),
    ?_assertMatch([{start_document, _},
            {start_element, {"", "tag", "tag"}, [], _},
            {end_element, {"", "tag", "tag"}, _},
            {end_document, _}],
        get_trace([<<"<tag />">>])),
    ?_assertMatch([{start_document, _},
            {start_element, {"", "tag", "tag"}, [], _},
            {end_element, {"", "tag", "tag"}, _},
            {end_document, _}],
        get_trace([<<"<tag></tag>">>])),
    ?_assertMatch([{start_document, _},
            {start_element, {"", "tag", "tag"}, [], _},
            {end_element, {"", "tag", "tag"}, _},
            {end_document, _}],
        get_trace([<<"<tag ></tag >">>])),
    ?_assertMatch([{start_document, _},
            {start_element, {"", "tag", "tag"}, [], _},
            {characters, "Data", _},
            {end_element, {"", "tag", "tag"}, _},
            {end_document, _}],
        get_trace([<<"<tag>Data</tag>">>])),
    ?_assertMatch([{start_document, _},
            {start_element, {"", "a", "a"}, [], _},
            {characters, "A", _},
            {start_element, {"", "b", "b"}, [], _},
            {characters, "B", _},
            {start_element, {"", "c", "c"}, [], _},
            {end_element, {"", "c", "c"}, _},
            {end_element, {"", "b", "b"}, _},
            {end_element, {"", "a", "a"}, _},
            {end_document, _}],
        get_trace([<<"<a>A<b>B<c/></b></a>">>]))
    ]}.


simple_attributes_test_() -> {setup, fun setup/0, fun cleanup/1, [
    ?_assertMatch([{start_document, _},
            {start_element, {"", "tag", "tag"},
                [{{"", "name", "name"}, "value"}], _},
            {end_element, {"", "tag", "tag"}, _},
            {end_document, _}],
        get_trace([<<"<tag name='value'/>">>])),
    ?_assertMatch([{start_document, _},
            {start_element, {"", "tag", "tag"},
                [{{"", "name", "name"}, " value "}], _},
            {end_element, {"", "tag", "tag"}, _},
            {end_document, _}],
        get_trace([<<"<tag name = ' value ' />">>])),
    ?_assertMatch([{start_document, _},
            {start_element, {"", "tag", "tag"},
                [{{"", "n1", "n1"}, "v1"}, {{"", "n2", "n2"}, "v2"}], _},
            {end_element, {"", "tag", "tag"}, _},
            {end_document, _}],
        get_trace([<<"<tag n1='v1' n2=\"v2\" />">>]))
    ]}.


continuation_test_() -> {setup, fun setup/0, fun cleanup/1, [
    ?_assertMatch([{start_document, _},
            {start_element, {"", "tag", "tag"}, [], _},
            {characters, "Da", _},
            {characters, "ta", _},
            {end_element, {"", "tag", "tag"}, _},
            {end_document, _}],
        get_trace([<<"<tag>Da">>, <<"ta</tag>">>])),
    ?_assertMatch([{start_document, _},
            {start_element, {"", "tag", "tag"}, [], _},
            {end_element, {"", "tag", "tag"}, _},
            {end_document, _}],
        get_trace([<<"<ta">>, <<"g/>">>])),
    ?_assertMatch([{start_document, _},
            {start_element, {"", "tag", "tag"},
                [{{"", "name", "name"}, "value"}], _},
            {end_element, {"", "tag", "tag"}, _},
            {end_document, _}],
        get_trace([<<"<">>, <<"ta">>, <<"g">>, <<" name">>, <<"='">>,
            <<"value">>, <<"'/>">>])),
    ?_assertMatch([{start_document, _},
            {start_element, {"", "a", "a"}, [], _},
            {start_element, {"", "b", "b"}, [], _},
            {end_element, {"", "b", "b"}, _},
            {start_element, {"", "c", "c"}, [], _},
            {end_element, {"", "c", "c"}, _},
            {end_element, {"", "a", "a"}, _},
            {end_document, _}],
        get_trace([<<"<a><b/><c">>, <<"/></a>">>])),
    ?_assertMatch([{start_document, _},
            {start_element, {"", "a", "a"}, [{{"", "n", "n"}, "v"}], _},
            {end_element, {"", "a", "a"}, _},
            {end_document, _}],
        get_trace([<<"<a n='v' ">>, <<" ></a>">>])),
    ?_assertMatch([{start_document, _},
            {start_element, {"", "a", "a"}, [], _},
            {end_element, {"", "a", "a"}, _},
            {end_document, _}],
        get_trace([<<"<a></a ">>, <<" >">>])),
    ?_assertMatch([{start_document, _},
            {start_element, {"", "a", "a"}, [], _},
            {characters, "\n", _},
            {characters, "a", _},
            {characters, "\n", _},
            {characters, "\n", _},
            {end_element, {"", "a", "a"}, _},
            {end_document, _}],
        get_trace(get_chars([<<"<a>\r\na\n\r</a>">>])))
    ]}.


comments_test_() -> {setup, fun setup/0, fun cleanup/1, [
    ?_assertMatch([{start_document, _},
            {start_element, {"", "tag", "tag"}, [], _},
            {end_element, {"", "tag", "tag"}, _},
            {end_document, _}],
        get_trace([<<"<tag><!-- Comment --></tag>">>]))
    ]}.


processing_instructions_test_() -> {setup, fun setup/0, fun cleanup/1, [
    ?_assertMatch([{start_document, _},
            {start_element, {"", "tag", "tag"}, [], _},
            {end_element, {"", "tag", "tag"}, _},
            {end_document, _}],
        get_trace([<<"<tag><?target?></tag>">>]))
    ]}.


cdata_test_() -> {setup, fun setup/0, fun cleanup/1, [
    ?_assertMatch([{start_document, _},
            {start_element, {"", "tag", "tag"}, [], _},
            {characters, "<tag>Data</tag>", _},
            {end_element, {"", "tag", "tag"}, _},
            {end_document, _}],
        get_trace([<<"<tag><![CDATA[<tag>Data</tag>]]></tag>">>])),
    ?_assertMatch([{start_document, _},
            {start_element, {"", "tag", "tag"}, [], _},
            {characters, "&#10;&#xf;&lt;", _},
            {end_element, {"", "tag", "tag"}, _},
            {end_document, _}],
        get_trace([<<"<tag><![CDATA[&#10;&#xf;&lt;]]></tag>">>]))
    ]}.


location_test_() -> {setup, fun setup/0, fun cleanup/1, [
        ?_assertEqual([
            {start_document, #location{source="test", line=1, column=1}},
            {start_element, {"", "a", "a"}, [],
                #location{source="test", line=1, column=1}},
            {characters, " A \n ",
                #location{source="test", line=1, column=4}},
            {start_element, {"", "b", "b"}, [],
                #location{source="test", line=2, column=2}},
            {characters, " B \n ",
                #location{source="test", line=2, column=5}},
            {end_element, {"", "b", "b"},
                #location{source="test", line=3, column=2}},
            {characters, " \n ",
                #location{source="test", line=3, column=6}},
            {end_element, {"", "a", "a"},
                #location{source="test", line=4, column=2}},
            {end_document, #location{source="test", line=4, column=6}}
            ], get_trace([<<"<a> A \r\n <b> B \n </b> \r </a>">>])),
        ?_assertEqual([
            {start_document, #location{source="test", line=1, column=1}},
            {characters, " \n ", #location{source="test", line=2, column=5}},
            {characters, " \n ", #location{source="test", line=3, column=7}},
            {characters, "B \n ", #location{source="test", line=4, column=2}},
            {start_element, {"", "a", "a"}, [],
                #location{source="test", line=5, column=5}},
            {end_element, {"", "a", "a"},
                #location{source="test", line=5, column=5}},
            {end_document, #location{source="test", line=5, column=9}}
            ], get_trace([<<"<!-- \n --> \n <?a?>">>,
                <<" \r\n <![CDATA[B \r ]]><a/>">>]))
    ]}.


reference_test_() -> {setup, fun setup/0, fun cleanup/1, [
    ?_assertMatch([{start_document, _},
            {start_element, {"", "a", "a"}, [], _},
            {characters, [10, 10], _},
            {end_element, {"", "a", "a"}, _},
            {end_document, _}],
        get_trace([<<"<a>&#10;&#xa;</a>">>])),
    ?_assertMatch([{start_document, _},
            {start_element, {"", "a", "a"}, [], _},
            {characters, "<>&'\"", _},
            {end_element, {"", "a", "a"}, _},
            {end_document, _}],
        get_trace([<<"<a>&lt;&gt;&amp;&apos;&quot;</a>">>])),
    ?_assertMatch([{start_document, _},
            {start_element, {"", "a", "a"},
                [{{"", "name", "name"}, "<>&'\""}], _},
            {end_element, {"", "a", "a"}, _},
            {end_document, _}],
        get_trace([<<"<a name='&lt;&gt;&amp;&apos;&quot;'/>">>])),
    ?_assertMatch([{start_document, _},
            {start_element, {"", "title", "title"}, [], _},
            {characters, "Less: <em> &lt; </em>", _},
            {end_element, {"", "title", "title"}, _},
            {end_document, _}],
        get_trace([<<"<title>Less: &lt;em> &amp;lt; &lt;/em></title>">>]))
    ]}.


reference_errors_test_() -> {setup, fun setup/0, fun cleanup/1, [
    ?_assertError({badref, #location{source="test", line=1, column=6}},
        get_trace([<<"<a>&#a;</a>">>])),
    ?_assertError({badref, #location{source="test", line=1, column=7}},
        get_trace([<<"<a>&#xg;</a>">>])),
    ?_assertError({badref, #location{source="test", line=1, column=6}},
        get_trace([<<"<a>&#;</a>">>])),
    ?_assertError({badref, #location{source="test", line=1, column=7}},
        get_trace([<<"<a>&#x;</a>">>])),
    ?_assertError({badref, #location{source="test", line=1, column=6}},
        get_trace([<<"<a>&#</a>">>])),
    ?_assertError({badref, #location{source="test", line=1, column=7}},
        get_trace([<<"<a>&#x</a>">>]))
    ]}.


reference_continuation_test_() -> {setup, fun setup/0, fun cleanup/1, [
    ?_assertMatch([{start_document, _},
            {start_element, {"", "a", "a"}, [], _},
            {characters, [100], _},
            {end_element, {"", "a", "a"}, _},
            {end_document, _}],
        get_trace([<<"<a>&#1">>, <<"00;</a>">>])),
    ?_assertMatch([{start_document, _},
            {start_element, {"", "a", "a"}, [], _},
            {characters, [16#fff], _},
            {end_element, {"", "a", "a"}, _},
            {end_document, _}],
        get_trace([<<"<a>&#xf">>, <<"ff;</a>">>])),
    ?_assertMatch([{start_document, _},
            {start_element, {"", "a", "a"}, [], _},
            {characters, "'", _},
            {end_element, {"", "a", "a"}, _},
            {end_document, _}],
        get_trace([<<"<a>&ap">>, <<"os;</a>">>]))
    ]}.


namespaces_test_() -> {setup, fun setup/0, fun cleanup/1, [
    ?_assertMatch([{start_document, _},
            {start_element, {"uri:test", "tag", "tag"},
                [{{"", "xmlns", "xmlns"},
                    "uri:test"}], _},
            {end_element, {"uri:test", "tag", "tag"}, _},
            {end_document, _}],
        get_trace([<<"<tag xmlns='uri:test'/>">>])),
    ?_assertMatch([{start_document, _},
            {start_element, {"uri:test", "tag", "ns:tag"},
                [{{"http://www.w3.org/2000/xmlns/", "ns", "xmlns:ns"},
                    "uri:test"}], _},
            {end_element, {"uri:test", "tag", "ns:tag"}, _},
            {end_document, _}],
        get_trace([<<"<ns:tag xmlns:ns='uri:test'/>">>])),
    ?_assertMatch([{start_document, _},
            {start_element, {"uri:test", "tag", "ns:tag"},
                [{{"http://www.w3.org/2000/xmlns/", "ns", "xmlns:ns"},
                    "uri:test"}], _},
            {end_element, {"uri:test", "tag", "ns:tag"}, _},
            {end_document, _}],
        get_trace([<<"<ns:tag xmlns:ns='uri:test'></ns:tag>">>])),
    ?_assertMatch([{start_document, _},
            {start_element, {"uri:test", "tag", "ns:tag"},
                [{{"http://www.w3.org/2000/xmlns/", "ns", "xmlns:ns"},
                    "uri:test"}], _},
            {start_element, {"uri:test", "a", "ns:a"}, [], _},
            {end_element, {"uri:test", "a", "ns:a"}, _},
            {end_element, {"uri:test", "tag", "ns:tag"}, _},
            {end_document, _}],
        get_trace([<<"<ns:tag xmlns:ns='uri:test'><ns:a/></ns:tag>">>])),
    ?_assertMatch([{start_document, _},
            {start_element, {"uri:test", "tag", "ns:tag"},
                [{{"http://www.w3.org/2000/xmlns/", "ns", "xmlns:ns"},
                    "uri:test"}], _},
            {start_element, {"uri:new", "a", "ns:a"},
                [{{"http://www.w3.org/2000/xmlns/", "ns", "xmlns:ns"},
                    "uri:new"}], _},
            {end_element, {"uri:new", "a", "ns:a"}, _},
            {end_element, {"uri:test", "tag", "ns:tag"}, _},
            {end_document, _}],
        get_trace([<<"<ns:tag xmlns:ns='uri:test'>">>,
            <<"<ns:a xmlns:ns='uri:new'/></ns:tag>">>])),
    ?_assertMatch([{start_document, _},
            {start_element, {"uri:test", "tag", "ns:tag"},
                [{{"http://www.w3.org/2000/xmlns/", "ns", "xmlns:ns"},
                    "uri:test"}], _},
            {start_element, {"uri:new", "a", "ns:a"},
                [{{"http://www.w3.org/2000/xmlns/", "ns", "xmlns:ns"},
                    "uri:new"}], _},
            {end_element, {"uri:new", "a", "ns:a"}, _},
            {end_element, {"uri:test", "tag", "ns:tag"}, _},
            {end_document, _}],
        get_trace([<<"<ns:tag xmlns:ns='uri:test'>">>,
            <<"<ns:a xmlns:ns='uri:new'></ns:a></ns:tag>">>])),
    ?_assertMatch([{start_document, _},
            {start_element, {"uri:test", "tag", "tag"},
                [{{"", "xmlns", "xmlns"}, "uri:test"}], _},
        {start_element, {"", "a", "a"},
                [{{"", "xmlns", "xmlns"}, ""}], _},
            {end_element, {"", "a", "a"}, _},
            {end_element, {"uri:test", "tag", "tag"}, _},
            {end_document, _}],
        get_trace([<<"<tag xmlns='uri:test'>">>,
            <<"<a xmlns=''></a></tag>">>])),
    ?_assertMatch([{start_document, _},
            {start_element, {"http://www.w3.org/XML/1998/namespace",
                "tag", "xml:tag"}, [], _},
            {end_element, {"http://www.w3.org/XML/1998/namespace",
                "tag", "xml:tag"}, _},
            {end_document, _}],
        get_trace([<<"<xml:tag/>">>])),
    ?_assertMatch([{start_document, _},
            {start_element, {"uri:test", "tag", "tag"},
                [{{"", "xmlns", "xmlns"}, "uri:test"},
                    {{"", "name", "name"}, "value"}], _},
            {end_element, {"uri:test", "tag", "tag"}, _},
            {end_document, _}],
        get_trace([<<"<tag xmlns='uri:test' name='value'/>">>])),
    ?_assertMatch([{start_document, _},
            {start_element, {"uri:test", "tag", "ns:tag"},
                [{{"http://www.w3.org/2000/xmlns/", "ns", "xmlns:ns"},
                    "uri:test"},
                    {{"uri:test", "name", "ns:name"}, "value"}], _},
            {end_element, {"uri:test", "tag", "ns:tag"}, _},
            {end_document, _}],
        get_trace([<<"<ns:tag xmlns:ns='uri:test' ns:name='value'/>">>]))
    ]}.


namespaces_error_test_() -> {setup, fun setup/0, fun cleanup/1, [
    ?_assertError({badns, #location{source="test", line=1, column=1}},
        get_trace([<<"<a xmlns:a:b='attr'/>">>])),
    ?_assertError({badns, #location{source="test", line=1, column=1}},
        get_trace([<<"<ns:a:b/>">>])),
    ?_assertError({badns, #location{source="test", line=1, column=1}},
        get_trace([<<"<ns:a/>">>])),
    ?_assertError({badns, #location{source="test", line=1, column=1}},
        get_trace([<<"<ns:a xmlns='uri:test'/>">>])),
    ?_assertError({badns, #location{source="test", line=1, column=1}},
        get_trace([<<"<xmlns:a xmlns:xmlns='uri:test'/>">>]))
    ]}.


xml_declaration_test_() -> {setup, fun setup/0, fun cleanup/1, [
    ?_assertMatch([{start_document, _},
            {start_element, {"", "tag", "tag"}, [], _},
            {characters, [16#442,16#435,16#441,16#442], _},
            {end_element, {"", "tag", "tag"}, _},
            {end_document, _}],
        get_trace([<<"<tag>">>,
            <<16#d1,16#82,16#d0,16#b5,16#d1,16#81,16#d1,16#82>>,
            <<"</tag>">>])),
    ?_assertMatch([{start_document, _},
            {start_element, {"", "tag", "tag"}, [], _},
            {characters, [16#442,16#435,16#441,16#442], _},
            {end_element, {"", "tag", "tag"}, _},
            {end_document, _}],
        get_trace([<<"<?xml version='1.0' encoding='utf-8'?><tag>">>,
            <<16#d1,16#82,16#d0,16#b5,16#d1,16#81,16#d1,16#82>>,
            <<"</tag>">>])),
    ?_assertMatch([{start_document, _},
            {start_element, {"", "tag", "tag"}, [], _},
            {characters, [16#442,16#435,16#441,16#442], _},
            {end_element, {"", "tag", "tag"}, _},
            {end_document, _}],
        get_trace([<<"<?xml version='1.0' encoding='windows-1251'?><tag>">>,
            <<16#f2,16#e5,16#f1,16#f2>>, <<"</tag>">>])),
    ?_assertMatch([{start_document, _},
            {start_element, {"", "tag", "tag"}, [], _},
            {characters, [16#442,16#435,16#441,16#442], _},
            {end_element, {"", "tag", "tag"}, _},
            {end_document, _}],
        get_trace([<<"<">>,<<"?">>,<<"xml">>,<<" version">>,<<"=">>,
            <<"'1.0'">>,<<" encoding">>,<<"='windows">>,<<"-1251">>,<<"'">>,
            <<"?">>,<<">">>,<<"<tag>">>, <<16#f2,16#e5,16#f1,16#f2>>,
            <<"</tag>">>]))
    ]}.


xml_declaration_error_test_() -> {setup, fun setup/0, fun cleanup/1, [
    ?_assertError({badtag, #location{source="test", line=1, column=4}},
        get_trace([<<"<a><?xml version='1.0'?></a>">>])),
    ?_assertError({bad_encoding, #location{source="test", line=1, column=1}},
        get_trace([<<"<?xml version='1.0' encoding='unknown'?><a/>">>]))
    ]}.
