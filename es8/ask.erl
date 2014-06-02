-module(ask).
-export([start_ask/0, ask/3]).

% Public API

% erl -sname test1 -setcookie abc
% es8:start_ask().
start_ask() -> 
    Pid = spawn(fun loop_ask/0),
    register(server, Pid),
    on_exit_ask(Pid, fun start_ask/0).

% erl -sname test2 -setcookie abc
% es8:ask(test1@hostname, fun(X) -> X * X end, 8).
% es8:ask(test1@hostname, fun(X) -> X * X end, aaa).
% es8:ask(test1@hostname, fun(X) -> X * X end, 8).
ask(Host, Fun, Num) ->
    {server, Host} ! {self(), Fun, Num},
    receive
        Reply ->
            Reply
    after 100 ->
        'Server crashed'
    end.

% Private API

loop_ask() ->
    receive
        {Sender, Fun, Num} ->
            io:format("~p~n", [Sender]),
            Sender ! Fun(Num),    
            loop_ask()
    end.

on_exit_ask(Pid, Fun) ->    
    spawn(fun() ->
        process_flag(trap_exit, true),
        link(Pid),
        receive
            {'EXIT', Pid, _} ->
                io:format("Error trapped\n"),
                Fun()
        end
    end).
