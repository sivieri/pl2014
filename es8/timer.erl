-module(timer).
-export([start/2, stop/0]).

% Public API

start(Time, Fun) ->
    register(clock, spawn(fun() -> tick(Time, Fun) end)).

stop() ->
    clock ! stop.

% Private API

tick(Time, Fun) ->
    receive
        stop ->
            ok
    after Time ->
        Fun(),
        tick(Time, Fun)
    end.
