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
%% @doc URL handling library
%%
-module(urllib).
-author("Dmitry Vasiliev <dima@hlabs.spb.ru>").
-vsn("0.3").

-export([urlsplit/1]).

-define(URL, "^(?:([^:]+)://)?([^/:]+)(?::([^/]+))?(.*)").


%%
%% @doc Split URL to {scheme, host, port, path}
%% @spec urlsplit(Url) -> Result
%%      Url = string()
%%      Result = Parts
%%      Parts = {Scheme, NetLoc, Port, Path}
%%      Scheme = http | https
%%      NetLoc = string()
%%      Port = integer()
%%      Path = string()
%%
urlsplit(Url) ->
    case re:run(Url, ?URL, [{capture, all_but_first, list}]) of
        {match, [S, NetLoc, P, Path]} ->
            Scheme = parse_scheme(string:to_lower(S)),
            {Scheme, NetLoc, parse_port(P, Scheme), get_path(Path)};
        _Other ->
            erlang:error(bad_url, [Url])
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
parse_scheme(Scheme) ->
    erlang:error(bad_url_scheme, [Scheme]).

%%
%% @doc Parse port
%%
parse_port("", http) ->
    80;
parse_port("", https) ->
    443;
parse_port(Port, Scheme) ->
    try list_to_integer(Port)
    catch
        error:badarg ->
            erlang:error(bad_url_port, [Port, Scheme])
    end.

%%
%% @doc Normalize path
%%
get_path("") ->
    "/";
get_path(Path) ->
    Path.
