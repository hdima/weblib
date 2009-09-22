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


get_trace(Chunk, Behaviour, Args) ->
    {ok, Trace} = xml:parse(Chunk, Behaviour, Args),
    lists:reverse(Trace).


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
    [{start_document},
        {start_element, "tag", []},
        {end_element, "tag"},
        {end_document}] = get_trace(<<"<tag/>">>, ?MODULE, []),
    [{start_document},
        {start_element, "tag", []},
        {end_element, "tag"},
        {end_document}] = get_trace(<<"<tag />">>, ?MODULE, []),
    [{start_document},
        {start_element, "tag", []},
        {end_element, "tag"},
        {end_document}] = get_trace(<<"<tag></tag>">>, ?MODULE, []),
    [{start_document},
        {start_element, "tag", []},
        {end_element, "tag"},
        {end_document}] = get_trace(<<"<tag ></tag >">>, ?MODULE, []),
    ok.


test_simple_attributes() ->
    [{start_document},
        {start_element, "tag", [{"name", "value"}]},
        {end_element, "tag"},
        {end_document}] = get_trace(<<"<tag name='value'/>">>, ?MODULE, []),
    [{start_document},
        {start_element, "tag", [{"name", " value "}]},
        {end_element, "tag"},
        {end_document}] = get_trace(
            <<"<tag name = ' value ' />">>, ?MODULE, []),
    [{start_document},
        {start_element, "tag", [{"n1", "v1"}, {"n2", "v2"}]},
        {end_element, "tag"},
        {end_document}] = get_trace(
            <<"<tag n1='v1' n2='v2' />">>, ?MODULE, []),
    ok.


test() ->
    test_constants(),
    test_errors(),
    test_simple_xml(),
    test_simple_attributes(),
    ok.
