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
%% @doc Crawler
%%
-module(crawler).
-author("Dmitry Vasiliev <dima@hlabs.spb.ru>").
-vsn("0.1").

%% Public interface
-export([crawl/2, crawl/3, crawl/4, start/0, start_link/0, stop/0]).

%% Protected interface
-export([get_next_url/0]).

%% Behaviour callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).


% TODO: Move to crawl/3 options
-define(SLEEP_TIMEOUT, 3 * 60 * 1000).


%%
%% @doc Crawl URL with handler
%% @spec crawl(Url, Fun) -> ok
%%      Url = string()
%%      Fun = function()
%%
crawl(Url, Fun) when is_list(Url) ->
    crawl(url:urlsplit(Url), Fun);
crawl(Url, Fun) when is_tuple(Url) ->
    gen_server:call(?MODULE, {crawl, Url, Fun}).


%%
%% @doc Crawl URL with handler
%% @spec crawl(Url, Fun, Args) -> ok
%%      Url = string()
%%      Fun = function()
%%      Args = list()
%%
crawl(Url, Fun, Args) ->
    crawl(Url, fun () -> Fun(Args) end).


%%
%% @doc Crawl URL with handler
%% @spec crawl(Url, Module, Fun, Args) -> ok
%%      Url = string()
%%      Module = module()
%%      Fun = function()
%%      Args = list()
%%
crawl(Url, Module, Fun, Args) ->
    crawl(Url, fun () -> Module:Fun(Args) end).


%%
%% @doc Get next URL to crawl (only for internal subprocesses)
%%
get_next_url() ->
    gen_server:call(?MODULE, get_next_url).


%%
%% @doc Start crawler
%%
start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).


%%
%% @doc Start crawler
%%
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%
%% @doc Stop crawler
%%
stop() ->
    gen_server:cast(?MODULE, stop).


%%
%% @doc Initialise process
%%
init([]) ->
    process_flag(trap_exit, true),
    % Ignore duplicate values
    Hosts = ets:new(crawl_hosts, [set, private]),
    Funs = ets:new(crawl_funs, [bag, private]),
    {ok, {Hosts, Funs}}.

terminate(_Reason, {Hosts, Funs}) ->
    ets:delete(Hosts),
    ets:delete(Funs),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%
%% Message handling
%%

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.


handle_call({crawl, {_, Host, _, _}, Fun}, _From, {Hosts, Funs}=State) ->
    case ets:lookup(Funs, Host) of
        [Fun | _] ->
            ets:insert(Funs, {Host, Fun});
        [] ->
            Pid = proc_lib:spawn_link(fun handler/0),
            ets:insert(Hosts, {Pid, Host}),
            ets:insert(Funs, {Host, Fun})
    end,
    {reply, ok, State};
handle_call(get_next_url, From, {Hosts, Funs}=State) ->
    Result = case ets:lookup(Hosts, From) of
        [{From, Host}] ->
            case ets:lookup(Funs, Host) of
                [{Host, Fun}=I | _] ->
                    ets:delete_object(Funs, I),
                    Fun;
                [] ->
                    empty
            end;
        [] ->
            % Request from unknown process
            badarg
    end,
    {reply, Result, State};
handle_call(_, _, State) ->
    {reply, badarg, State}.


handle_info({'EXIT', From, normal}, {Hosts, _}=State) ->
    ets:delete(Hosts, From),
    {noreply, State};
handle_info({'EXIT', From, _Reason}, {Hosts, Funs}=State) ->
    case ets:lookup(Funs, From) of
        [{From, Host}] ->
            % Restart handler
            ets:delete(Hosts, From),
            Pid = proc_lib:spawn_link(fun handler/0),
            ets:insert(Hosts, {Pid, Host});
        [] ->
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.


handler() ->
    timer:sleep(?SLEEP_TIMEOUT),
    case get_next_url() of
        empty ->
            ok;
        Fun ->
            Fun(),
            handler()
    end.
