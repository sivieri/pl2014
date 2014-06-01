-module(es7).
-export([len/1, lent/1, range/2, ranget/2, map/2, right_triangles/0, takewhile/2, pack/1, encode/1]).
-export([tree_singleton/1, tree_insert/2, tree_elem/2, tree_sum/1, tree_values/1, tree_map/2, tree_fold/3, tree_values2/1]).
-record(node, {value = nil, left = nil, right = nil}).

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

tree_singleton(Value) ->
    #node{value = Value, left = tree_empty(), right = tree_empty()}.

% Nums = [8,6,4,1,7,3,5].
% lists:foldr(fun es7:tree_insert/2, es7:tree_empty(), Nums).
tree_insert(Value, #node{value = nil, left = nil, right = nil}) ->
    tree_singleton(Value);
tree_insert(Value, #node{value = CurValue, left = _Left, right = _Right} = Tree) when Value == CurValue ->
    Tree;
tree_insert(Value, #node{value = CurValue, left = Left, right = _Right} = Tree) when Value < CurValue ->
    NewLeft = tree_insert(Value, Left),
    Tree#node{left = NewLeft};
tree_insert(Value, #node{value = _CurValue, left = _Left, right = Right} = Tree) ->
    NewRight = tree_insert(Value, Right),
    Tree#node{right = NewRight}.

tree_elem(_Searched, #node{value = nil, left = nil, right = nil}) ->
    false;
tree_elem(Searched, #node{value = Value, left = _Left, right = _Right}) when Searched == Value ->
    true;
tree_elem(Searched, #node{value = Value, left = Left, right = _Right}) when Searched < Value ->
    tree_elem(Searched, Left);
tree_elem(Searched, #node{value = _Value, left = _Left, right = Right}) ->
    tree_elem(Searched, Right).

tree_sum(#node{value = nil, left = nil, right = nil}) ->
    0;
tree_sum(#node{value = Value, left = Left, right = Right}) ->
    Value + tree_sum(Left) + tree_sum(Right).

tree_values(#node{value = nil, left = nil, right = nil}) ->
    [];
tree_values(#node{value = Value, left = Left, right = Right}) ->
    [Value|tree_values(Left)] ++ tree_values(Right).

tree_map(_F, #node{value = nil, left = nil, right = nil}) ->
    tree_empty();
tree_map(F, #node{value = Value, left = Left, right = Right}) ->
    #node{value = F(Value), left = tree_map(F, Left), right = tree_map(F, Right)}.

tree_fold(_F, Acc, #node{value = nil, left = nil, right = nil}) ->
    Acc;
tree_fold(F, Acc, #node{value = Value, left = Left, right = Right}) ->
    tree_fold(F, F(tree_fold(F, Acc, Left), Value), Right).

tree_values2(Tree) ->
    tree_fold(fun(Acc, Cur) -> Acc ++ Cur end, [], tree_map(fun(X) -> [X] end, Tree)).

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

tree_empty() ->
    #node{}.
