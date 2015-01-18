# eredis_smart_sub

Layer on top of [eredis](https://github.com/wooga/eredis) to multiplex pub connections among several subscriptor processes in pubsub context

eredis, a more low-level approach to redis pubsub, requires a process to be the controller of every subscription done in a connection. However, if you have multiple processes that want to subscribe to different channels, you are faced with a decission:

1 Open a redis connection for each project that wants to subscribe to channels. However, this can cause you to open several connections to redis. There are some issues here: 
  * While cheap, redis connections are not free.
  * A linux machine is capped to 65535 ports.
  * This kind of problems are not the type one likes to face in the middle of the night. Or having in your head.
  * Opening a connection for every erlang process does not feel very clean, does it?
2 Write a layer on top of eredis with a process that will receive all the messages and send them back to the processes that are listening to the corresponding channels.

eredis_pub_sub implements this second approach, providing that layer.

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

 
