-module(es7).
-export([len/1, lent/1, range/2, ranget/2, map/2, right_triangles/0, takewhile/2, pack/1, encode/1]).

% Public API

len([]) ->
    0;
len([_|T]) ->
    1 + len(T).

lent(List) ->
    lent_helper(List, 0).

range(Low, High) when Low > High ->
    erlang:error(badarg);
range(Low, High) when Low == High ->
    [Low];
range(Low, High) ->
    [Low|range(Low + 1, High)].

ranget(Low, High) when Low > High ->
    erlang:error(badarg);
ranget(Low, High) ->
    lists:reverse(ranget_helper(Low, High, [])).

map(_F, []) ->
    [];
map(F, [H|T]) ->
    [F(H)|map(F, T)].

right_triangles() ->
    [{A, B, C}|| C <- range(1, 10), B <- range(1, C), A <- range(1, B), A * A + B * B == C * C].

takewhile(_F, []) ->
    [];
takewhile(F, [H|T]) ->
    case F(H) of
        true ->
            [H|takewhile(F, T)];
        false ->
            []
    end.

pack(List) ->
    lists:reverse(pack_helper(List, [], [])).

encode(List) ->
    Packed = pack(List),
    lists:zip(lists:map(fun hd/1, Packed), lists:map(fun length/1, Packed)).

% Private API

lent_helper([], Acc) ->
    Acc;
lent_helper([_|T], Acc) ->
    lent_helper(T, 1 + Acc).

ranget_helper(Low, High, Acc) when Low == High ->
    [Low|Acc];
ranget_helper(Low, High, Acc) ->
    ranget_helper(Low + 1, High, [Low|Acc]).

pack_helper([], Acc, Sub) ->
    [Sub|Acc];
pack_helper([H|T], Acc, []) ->
    pack_helper(T, Acc, [H]);
pack_helper([H1|T1], Acc, [H2|_T2] = Sub) when H1 == H2 ->
    pack_helper(T1, Acc, [H1|Sub]);
pack_helper([H1|T1], Acc, Sub) ->
    pack_helper(T1, [Sub|Acc], [H1]).
