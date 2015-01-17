-module(eredis_smart_sub_tests).

-include_lib("eunit/include/eunit.hrl").

start_test_() ->
    {"It should be possible to start",
     {setup,
      fun start_eredis_sub/0,
      fun(Client) ->
              [start_and_test_running(Client)]
      end}}.

subscribe_test_() ->
    {"It should be possible to subscribe to a channel",
     {setup,
      fun start_eredis_smart_sub/0,
      fun({Client, SubClient}) ->
              [test_subscribe(Client, SubClient)]
      end}}.

%%% Startups/Teardowns
start_eredis_sub() ->
    {ok, Client} = eredis_sub:start_link(),
    Client.

start_eredis_smart_sub() ->
    {ok, EredisSubClient} = eredis_sub:start_link(),
    {ok, SubClient} = eredis_smart_sub:start_link(EredisSubClient),
    {ok, Client} = eredis:start_link(),
    {Client, SubClient}.

%%% Helpers

add_channels(SubClient, Channels) ->
    gen_server:cast(SubClient, {subscribe, Channels, self()}),
    lists:foreach(
      fun (C) ->
              receive M ->
                      ?assertEqual({subscribed, C}, M)
              end
      end, Channels).


%%% Test functions
start_and_test_running(Client) ->
    Res = eredis_smart_sub:start_link(Client),
    ?_assertMatch({ok, _}, Res).

test_subscribe(Client, SubClient) ->
    add_channels(SubClient, [<<"chan1">>]),
    eredis:q(Client, ["PUBLISH", <<"chan1">>, msg]),
    Message = receive
                  {received_message, M} -> M
              after 1000 ->
                        throw(timeout)
              end,
    ?_assertEqual(<<"msg">>, Message).
