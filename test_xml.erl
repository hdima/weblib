-module(test_xml).

-export([parse_file/1]).

-behaviour(simplexml).

-export([start_document/1, end_document/1, start_element/3, end_element/2,
    characters/2]).


parse_file(Filename) ->
    {ok, File} = file:open(Filename, [read, raw, binary]),
    {continue, ParseState} = simplexml:parse(<<>>, ?MODULE, none),
    read_file(File, ParseState).


read_file(File, ParseState) ->
    case file:read(File, 4096) of
        {ok, Data} ->
            case simplexml:parse(Data, ParseState) of
                {continue, NewParseState} ->
                    read_file(File, NewParseState);
                {ok, _} ->
                    file:close(File)
            end;
        eof ->
            simplexml:parse(eof, ParseState),
            file:close(File)
    end.


start_document(Args) ->
    io:format("start document~n"),
    {ok, Args}.


end_document(State) ->
    io:format("end document~n"),
    {ok, State}.

start_element(Tag, Attributes, State) ->
    io:format("start element: ~p ~p~n", [Tag, Attributes]),
    {ok, State}.


end_element(Tag, State) ->
    io:format("end element: ~p~n", [Tag]),
    {ok, State}.


characters(Chunk, State) ->
    io:format("characters: ~p~n", [Chunk]),
    {ok, State}.
