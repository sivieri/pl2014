-module(pubsub).
-export([start_dispatcher/0, start_client/0, subscribe/2, publish/3]).

% Public API

start_client() ->
    Pid = spawn(fun client_loop/0),
    register(client, Pid).

subscribe(Host, Topic) ->
    {dispatcher, Host} ! {subscribe, node(), Topic}.

publish(Host, Topic, Message) ->
    {dispatcher, Host} ! {publish, Topic, Message}.

start_dispatcher() ->
    Pid = spawn(fun dispatcher_loop/0),
    register(dispatcher, Pid).

% Private API

client_loop() ->
    receive
        {Topic, Message} ->
            io:format("Received message ~p for topic ~p~n", [Message, Topic]),
            client_loop()
    end.

dispatcher_loop() -> 
    io:format("Dispatcher started~n"),
    dispatcher_loop(dict:new()).
dispatcher_loop(Interests) ->
    receive
        {subscribe, Client, Topic} ->
            dispatcher_loop(add_interest(Interests, Client, Topic));
        {publish, Topic, Message} ->
            Destinations = compute_destinations(Topic, Interests),
            send(Topic, Message, Destinations),
            dispatcher_loop(Interests)
    end.

compute_destinations(Topic, Interests) ->
    dict:fold(fun(Client, Current, AccIn) ->
                    case lists:member(Topic, Current) of
                        true -> [Client|AccIn];
                        false -> AccIn
                    end 
              end, [], Interests).

send(Topic, Message, Destinations) ->
    lists:foreach(fun(Client) -> {client, Client} ! {Topic, Message} end, Destinations).

add_interest(Interests, Client, Topic) ->
    dict:update(Client, fun(Current) -> [Topic|Current] end, [Topic], Interests).
