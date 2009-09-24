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

-include("xml.hrl").


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
%% Tests
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


test_constants() ->
    whitespace = get_char_type(16#20),
    whitespace = get_char_type(16#9),
    whitespace = get_char_type(16#D),
    whitespace = get_char_type(16#A),
    namestartchar = get_char_type($:),
    namestartchar = get_char_type($_),
    namestartchar = get_char_type($a),
    namestartchar = get_char_type($z),
    namestartchar = get_char_type($A),
    namestartchar = get_char_type($Z),
    namechar = get_char_type($-),
    namechar = get_char_type($.),
    namechar = get_char_type($0),
    namechar = get_char_type($9),
    attrvaluechar = get_char_type($?),
    ok.


get_trace(Chunk) ->
    Server = self(),
    {ok, {Server, N}} = xml:parse(Chunk, ?MODULE, {Server, 0}),
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


% FIXME: Check callbacks
test_errors() ->
    {'EXIT', {xml_nodata, _}} = (catch xml:parse(<<>>, ?MODULE, [])),
    {'EXIT', {xml_badtag, _}} = (catch xml:parse(<<"</>">>, ?MODULE, [])),
    {'EXIT', {xml_badtag, _}} = (catch xml:parse(<<"< tag/>">>, ?MODULE, [])),
    {'EXIT', {xml_badattr, _}} = (catch xml:parse(<<"<tag =/>">>, ?MODULE, [])),
    {'EXIT', {xml_badattr, _}} = (catch xml:parse(
        <<"<tag name/>">>, ?MODULE, [])),
    {'EXIT', {xml_badattr, _}} = (catch xml:parse(
        <<"<tag name name=/>">>, ?MODULE, [])),
    {'EXIT', {xml_badattr, _}} = (catch xml:parse(
        <<"<tag n1='v1'n2='v2'/>">>, ?MODULE, [])),
    ok.


test_simple_xml() ->
    [start_document,
        {start_element, "tag", []},
        {end_element, "tag"},
        end_document] = get_trace(<<"<tag/>">>),
    [start_document,
        {start_element, "tag", []},
        {end_element, "tag"},
        end_document] = get_trace(<<"<tag />">>),
    [start_document,
        {start_element, "tag", []},
        {end_element, "tag"},
        end_document] = get_trace(<<"<tag></tag>">>),
    [start_document,
        {start_element, "tag", []},
        {end_element, "tag"},
        end_document] = get_trace(<<"<tag ></tag >">>),
    [start_document,
        {start_element, "tag", []},
        {characters, "Data"},
        {end_element, "tag"},
        end_document] = get_trace(<<"<tag>Data</tag>">>),
    [start_document,
        {start_element, "a", []},
        {characters, "A"},
        {start_element, "b", []},
        {characters, "B"},
        {start_element, "c", []},
        {end_element, "c"},
        {end_element, "b"},
        {end_element, "a"},
        end_document] = get_trace(<<"<a>A<b>B<c/></b></a>">>),
    ok.


test_simple_attributes() ->
    [start_document,
        {start_element, "tag", [{"name", "value"}]},
        {end_element, "tag"},
        end_document] = get_trace(<<"<tag name='value'/>">>),
    [start_document,
        {start_element, "tag", [{"name", " value "}]},
        {end_element, "tag"},
        end_document] = get_trace(<<"<tag name = ' value ' />">>),
    [start_document,
        {start_element, "tag", [{"n1", "v1"}, {"n2", "v2"}]},
        {end_element, "tag"},
        end_document] = get_trace(<<"<tag n1='v1' n2=\"v2\" />">>),
    ok.


get_chunked_trace(Chunks) ->
    get_chunked_trace(Chunks, none).

get_chunked_trace([Chunk | Chunks], none) ->
    Server = self(),
    {continue, DocId} = xml:parse(Chunk, ?MODULE, {Server, 0}),
    get_chunked_trace(Chunks, {DocId, Server});
get_chunked_trace([Chunk | Chunks], {DocId, Server}) ->
    case xml:parse(Chunk, DocId) of
        {continue, NewId} ->
            get_chunked_trace(Chunks, {NewId, Server});
        {ok, {Server, N}} ->
            get_callbacks(N)
    end.


test_continuation() ->
    [start_document,
        {start_element, "tag", []},
        {characters, "Da"},
        {characters, "ta"},
        {end_element, "tag"},
        end_document] = get_chunked_trace([<<"<tag>Da">>, <<"ta</tag>">>]),
    [start_document,
        {start_element, "tag", []},
        {end_element, "tag"},
        end_document] = get_chunked_trace([<<"<ta">>, <<"g/>">>]),
    [start_document,
        {start_element, "tag", [{"name", "value"}]},
        {end_element, "tag"},
        end_document] = get_chunked_trace(
            [<<"<">>, <<"ta">>, <<"g">>, <<" name">>, <<"='">>,
                <<"value">>, <<"'/>">>]),
    % FIXME
    %[start_document,
    %    {start_element, "a", []},
    %    {start_element, "b", []},
    %    {end_element, "b"},
    %    {start_element, "c", []},
    %    {end_element, "c"},
    %    {end_element, "a"},
    %    end_document] = get_chunked_trace(
    %        [<<"<a><b/><c">>, <<"/></a>">>]),
    ok.


test() ->
    test_constants(),
    test_errors(),
    test_simple_xml(),
    test_simple_attributes(),
    test_continuation(),
    ok.
