-module(eredis_smart_sub).
-behaviour(gen_server).

%% API
-export([start_link/1, stop/1]).
-include_lib("eunit/include/eunit.hrl").

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {client, subscriptions}).

%%
%% PUBLIC API
%%

start_link(Client) ->
    gen_server:start_link(?MODULE, [Client], []).

stop(Pid) ->
    gen_server:call(Pid, stop).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Client]) ->
    eredis_sub:controlling_process(Client),
    State = #state{client = Client, subscriptions = dict:new()},
    {ok, State}.

handle_info(stop, State) ->
    {stop, shutdown, State};

handle_info({subscribed, _Channel, Pid}, State) ->
    eredis_sub:ack_message(Pid),
    {noreply, State};
handle_info({unsubscribed, _Channel, Pid}, State) ->
    eredis_sub:ack_message(Pid),
    {noreply, State};
handle_info({message, Channel, Msg, Pid}, #state{subscriptions = Subscriptions} = State) ->
    eredis_sub:ack_message(Pid),
    case dict:find(Channel, Subscriptions) of
        error -> ok;
        {ok, Subscribed} -> [ReplyTo ! {received_message, Msg} || ReplyTo <- Subscribed]
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {stop, {unhandled_message, _Info}, State}.

handle_cast({subscribe, Channels, From}, #state{client = Client, subscriptions = Subscriptions} = State) ->
    SubFun = fun(Channel, {Subs, Chans}) ->
                     case dict:find(Channel, Subs) of
                         error -> {dict:append(Channel, From, Subs), [Channel | Chans]};
                         _ -> {dict:append(Channel, From, Subs), Chans}
                     end
             end,
    {NewSubscriptions, NewChannels} = lists:foldl(SubFun, {Subscriptions, []}, Channels),
    eredis_sub:subscribe(Client, NewChannels),
    {noreply, State#state{subscriptions = NewSubscriptions}};

handle_cast({unsubscribe, Channels, From}, #state{client = Client, subscriptions = Subscriptions} = State) ->
    SubFun = fun(Channel, {Subs, Chans}) ->
                     case dict:find(Channel, Subs) of
                         error -> {Subs, Chans};
                         {ok, Subscribed} -> {dict:store(Channel, lists:delete(From, Subscribed), Subs), [Channel | Chans]}
                     end
             end,
    {NewSubscriptions, RemovedChannels} = lists:foldl(SubFun, {Subscriptions, []}, Channels),
    eredis_sub:unsubscribe(Client, RemovedChannels),
    {noreply, State#state{subscriptions = NewSubscriptions}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, unknown_request, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

