-module(xml).

-export([feed/2, feed/1]).


feed(Xml) ->
    Client = self(),
    spawn_monitor(fun () -> parse(Xml, Client) end).


feed(eof, {Server, Monitor}) ->
    Server ! {self(), <<>>},
    receive
        {Server, Result} ->
            Result;
        {'DOWN', Monitor, process, Server, Reason} ->
            {error, Reason}
    after
        100 ->
            timeout
    end;
feed(Xml, {Server, _}) ->
    Server ! {self(), Xml}.


parse(Xml, Client) ->
    Result = xmerl_sax_parser:stream(Xml, [
        {continuation_fun, fun next/1},
        {continuation_state, Client},
        {event_fun, fun handle_event/3}
    ]),
    Client ! {self(), Result}.


handle_event(Event, _Location, State) ->
    io:format("~p~n", [Event]),
    State.


next(Client) ->
    receive
        {Client, Xml} ->
            {Xml, Client}
    end.
