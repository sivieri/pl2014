-module(intproc).
-export([start/0, client/2]).

% Public API

start() ->
    InitialState = 0,
    spawn(fun() -> loop(InitialState) end).

client(Pid, incr) ->
    Pid ! {self(), incr};
client(Pid, Cmd) ->
    Pid ! {self(), Cmd},
    receive
        Value ->
            io:format("~p~n", [Value])
    end.

% Private API

loop(State) ->
    receive
        {Sender, get} ->
            Sender ! State,
            loop(State);
        {_Sender, incr} ->
            loop(State + 1);
        {Sender, incrget} ->
            NewState = State + 1,
            Sender ! NewState,
            loop(NewState)
    end.
