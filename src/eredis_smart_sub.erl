-module(eredis_smart_sub).
-behaviour(gen_server).

%% API
-export([start_link/1, stop/1]).
-include_lib("eunit/include/eunit.hrl").

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).



-record(state, {client, subscriptions, subscribers}).

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
    State = #state{client = Client, subscriptions = dict:new(), subscribers = gb_sets:empty()},
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
        {ok, Subscribed} -> [ReplyTo ! {message, Msg} || ReplyTo <- Subscribed]
    end,
    {noreply, State};
handle_info({'DOWN', _Ref, process, Pid, _}, State = #state{subscriptions = Subscriptions, subscribers = Subscribers}) ->
    NewSubscribers = case gb_sets:is_member(Pid, Subscribers) of
                        false -> Subscribers;
                        true -> gb_sets:delete(Pid, Subscribers)
                    end,
    % Unsubscribe to his channels
    Channels = channels_with_only_subscriber(Pid, Subscriptions),
    gen_server:cast(?MODULE, {unsubscribe, Channels, Pid}),
    {noreply, State#state{subscribers = NewSubscribers}};
handle_info(_Info, State) ->
    {stop, {unhandled_message, _Info}, State}.
handle_cast({subscribe, Channels, From}, #state{client = Client, subscriptions = Subscriptions, subscribers = Subscribers} = State) ->
    SubFun = fun(Channel, {Subs, Chans}) ->
                     case dict:find(Channel, Subs) of
                         error -> {dict:append(Channel, From, Subs), [Channel | Chans]};
                         _ -> {dict:append(Channel, From, Subs), Chans}
                     end
             end,

    {NewSubscriptions, NewChannels} = lists:foldl(SubFun, {Subscriptions, []}, Channels),
    eredis_sub:subscribe(Client, NewChannels),

    NewSubscribers = case gb_sets:is_element(From, Subscribers) of
                         true -> Subscribers;
                         false -> erlang:monitor(process, From),
                                  gb_sets:insert(From, Subscribers)
                     end,
    {noreply, State#state{subscriptions = NewSubscriptions, subscribers = NewSubscribers}};

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

handle_call({subscribers, Channel}, _From, #state{subscriptions = Subscriptions} = State) ->
    Subscribers = case dict:find(Channel, Subscriptions) of
                      {ok, Subscribers1} -> Subscribers1;
                      error -> []
                  end,
    {reply, {ok, Subscribers}, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, unknown_request, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

channels_with_only_subscriber(Pid, Subscribers) ->
    dict:fold(fun(Channel, Subs, Channels) ->
            if Subs =:= [Pid] -> [Channel|Channels];
               true -> Channels
            end
        end, [], Subscribers).
