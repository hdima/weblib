%%
%% @doc URL handling library
%%
%% @author Dmitry Vasiliev <dima@hlabs.spb.ru>
%% @version 0.1
%%
-module(url).
-vsn(0.1).

-export([urlsplit/1]).

-define(URL, "^(?:([^:]+)://)?([^/:]+)(?::([^/]+))?(.*)").


%%
%% @doc Split URL to {scheme, host, port, path}
%% @spec urlsplit(Url) -> Result
%%      Url = string()
%%      Result = {ok, Parts} | {error, url} | {error, scheme} | {error, port}
%%      Parts = {Scheme, NetLoc, Port, Path}
%%      Scheme = http | https
%%      NetLoc = binary()
%%      Port = integer()
%%      Path = binary()
%%
urlsplit(Url) ->
    case re:run(Url, ?URL, [{capture, all_but_first, list}]) of
        {match, [Scheme, NetLoc, Port, Path]} ->
            case parse_scheme(string:to_lower(Scheme)) of
                unknown ->
                    {error, scheme};
                SchemeTok ->
                    case parse_port(Port, SchemeTok) of
                        unknown ->
                            {error, port};
                        PortNum ->
                            {ok, {SchemeTok, list_to_binary(NetLoc),
                                PortNum, list_to_binary(Path)}}
                    end
            end;
        _Other ->
            {error, url}
    end.

%%
%% @doc Parse scheme
%%
parse_scheme("") ->
    http;
parse_scheme("http") ->
    http;
parse_scheme("https") ->
    https;
parse_scheme(_) ->
    unknown.

%%
%% @doc Parse port
%%
parse_port("", http) ->
    80;
parse_port("", https) ->
    443;
parse_port(Port, _Scheme) when length(Port) > 0 ->
    list_to_integer(Port);
parse_port(_, _) ->
    unknown.
