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
-export([crawl/2, crawl/4, start/0, start/1,
    start_link/0, start_link/1, stop/0]).

%% Protected interface
-export([get_next_url/0]).

%% Behaviour callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).


-record(state, {
    timeout=5000,
    hosts,
    funs
    }).


%%
%% @doc Crawl URL with handler
%% @spec crawl(Url, Fun) -> ok
%%      Url = string()
%%      Fun = function()
%%
crawl(Url, Fun) when is_list(Url) ->
    crawl(urllib:urlsplit(Url), Fun);
crawl(Url, Fun) when is_tuple(Url) ->
    gen_server:call(?MODULE, {crawl, Url, Fun}).


%%
%% @doc Crawl URL with handler
%% @spec crawl(Url, Module, Fun, Args) -> ok
%%      Url = string()
%%      Module = atom()
%%      Function = atom()
%%      Args = list()
%%
crawl(Url, Module, Function, Args) ->
    crawl(Url, fun () -> Module:Function(Args) end).


%%
%% @doc Get next URL to crawl (only for internal subprocesses)
%%
get_next_url() ->
    gen_server:call(?MODULE, get_next_url).


%%
%% @doc Start crawler
%%
start() ->
    start_(#state{}).

start(Options) ->
    start_(parse_options(Options, #state{})).

start_(Options) ->
    gen_server:start({local, ?MODULE}, ?MODULE, [Options], []).

%%
%% @doc Start crawler
%%
start_link() ->
    start_link_(#state{}).

start_link(Options) ->
    start_link_(parse_options(Options, #state{})).

start_link_(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Options], []).


%%
%% @doc Stop crawler
%%
stop() ->
    gen_server:cast(?MODULE, stop).


parse_options([], Options) ->
    Options;
parse_options([{timeout, Timeout} | Tail], Options) ->
    parse_options(Tail, Options#state{timeout=Timeout});
parse_options([Option | _], _) ->
    erlang:error({badarg, Option}).


%%
%% @doc Initialise process
%%
init([Options]) ->
    process_flag(trap_exit, true),
    % Ignore duplicate values
    Hosts = ets:new(crawl_hosts, [set, private]),
    Funs = ets:new(crawl_funs, [bag, private]),
    {ok, Options#state{hosts=Hosts, funs=Funs}}.

terminate(_Reason, State) ->
    ets:delete(State#state.hosts),
    ets:delete(State#state.funs),
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


handle_call({crawl, {_, Host, _, _}, Fun}, _From, State) ->
    case ets:member(State#state.funs, Host) of
        false ->
            Proc = fun () -> handler(State) end,
            Pid = proc_lib:spawn_link(Proc),
            ets:insert(State#state.hosts, {Pid, Host}),
            ets:insert(State#state.funs, {Host, Fun});
        true ->
            ets:insert(State#state.funs, {Host, Fun})
    end,
    {reply, ok, State};
handle_call(get_next_url, {Pid, _}, State) ->
    Result = case ets:lookup(State#state.hosts, Pid) of
        [{Pid, Host}] ->
            case ets:lookup(State#state.funs, Host) of
                [{Host, Fun}=I | _] ->
                    ets:delete_object(State#state.funs, I),
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


handle_info({'EXIT', _, shutdown}, State) ->
    {stop, normal, State};
handle_info({'EXIT', Pid, normal}, State) ->
    ets:delete(State#state.hosts, Pid),
    {noreply, State};
handle_info({'EXIT', Pid, _Reason}, State) ->
    case ets:lookup(State#state.hosts, Pid) of
        [{Pid, Host}] ->
            % Restart handler
            ets:delete(State#state.hosts, Pid),
            Proc = fun () -> handler(State) end,
            NewPid = proc_lib:spawn_link(Proc),
            ets:insert(State#state.hosts, {NewPid, Host}),
            {noreply, State};
        [] ->
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.


handler(State) ->
    timer:sleep(State#state.timeout),
    case get_next_url() of
        empty ->
            ok;
        Fun ->
            Fun(),
            handler(State)
    end.
