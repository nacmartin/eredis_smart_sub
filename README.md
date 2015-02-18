# eredis_smart_sub

**eredis_smart_sub** is a layer on top of [eredis](https://github.com/wooga/eredis) to multiplex sub connections among several subscriptor processes in pubsub context

eredis, a more low-level approach to redis pubsub, requires a process to be the controller of every subscription done in a connection. However, if you have multiple processes that want to subscribe to different channels, you are faced with a decision:

- Open a Redis connection for each process that wants to subscribe to channels. However, this can cause you to have to open several connections to Eedis. There are some issues here: 
  * While cheap, redis connections are not free.
  * A linux machine is capped to 65535 ports.
  * This kind of problem is not the type one likes to face in the middle of the night.
  * Opening a connection for every Erlang process does not feel very clean, does it?
- Write a layer on top of eredis with a process that will receive all the messages and send them back to the processes that are listening to the corresponding channels.

eredis_smart_sub implements this second approach, providing that layer.

## Installation

With [rebar](https://github.com/basho/rebar):

Add this line to your deps in `rebar.config`:

```
 {eredis, ".*", {git, "git://github.com/wooga/eredis.git", {tag, "v0.2"}}}
```

## Usage

### Startup
Start eredis_sub

```erlang
{ok, EredisSubClient} = eredis_sub:start_link(),
% Start eredis_smart_sub with the eredis_sub client
% eredis_smart_sub will be assigned as the controller process of the eredis_sub client
{ok, SubClient} = eredis_smart_sub:start_link(EredisSubClient),
```

### Subscription

Now we can subscribe to channels from any process, and messages received in their channels  will be sent to these processes
```erlang
gen_server:cast(SubClient, {subscribe, [<<"channel1">>], self()}),
```
Now the process can receive messages.

Publish something to `channel1` and run

```erlang
 Message = receive
    {message, M} -> M
 end.
```

### Unsubscription

When the process is done with a channel can unsubscribe from it (or from a list of channels).

```erlang
gen_server:cast(SubClient, {unsubscribe, [<<"channel1">>], self()}),
```

### Handling of death of subscriptor processes
If the subscriber process dies `eredis_smart_sub` will send UNSUSCRIBE messages to redis for the channels where the process was the only subscriber. It is a bit more expensive, though, than simply unsubscribing explicitely from the channels we were subscribed to before termination. Of course, one never knows when a process is going to die in a pool of blood, so `eredis_smart_sub` has this covered.

## To build

- Get [rebar](https://github.com/basho/rebar).

```
rebar get-deps
rebar compile
```

## To run tests

```
rebar compile eunit skip_deps=true
```

