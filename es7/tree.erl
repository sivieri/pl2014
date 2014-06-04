-module(tree).
-export([tree_singleton/1, tree_insert/2, tree_elem/2, tree_sum/1, tree_values/1, tree_map/2, tree_fold/3, tree_values2/1]).
-record(node, {value = nil, left = nil, right = nil}).

% Public API

singleton(Value) ->
    #node{value = Value, left = tree_empty(), right = tree_empty()}.

% Nums = [8,6,4,1,7,3,5].
% lists:foldr(fun es7:tree_insert/2, es7:tree_empty(), Nums).
insert(Value, #node{value = nil, left = nil, right = nil}) ->
    singleton(Value);
insert(Value, #node{value = CurValue, left = _Left, right = _Right} = Tree) when Value == CurValue ->
    Tree;
insert(Value, #node{value = CurValue, left = Left, right = _Right} = Tree) when Value < CurValue ->
    NewLeft = insert(Value, Left),
    Tree#node{left = NewLeft};
insert(Value, #node{value = _CurValue, left = _Left, right = Right} = Tree) ->
    NewRight = insert(Value, Right),
    Tree#node{right = NewRight}.

elem(_Searched, #node{value = nil, left = nil, right = nil}) ->
    false;
elem(Searched, #node{value = Value, left = _Left, right = _Right}) when Searched == Value ->
    true;
elem(Searched, #node{value = Value, left = Left, right = _Right}) when Searched < Value ->
    elem(Searched, Left);
elem(Searched, #node{value = _Value, left = _Left, right = Right}) ->
    elem(Searched, Right).

sum(#node{value = nil, left = nil, right = nil}) ->
    0;
sum(#node{value = Value, left = Left, right = Right}) ->
    Value + sum(Left) + sum(Right).

values(#node{value = nil, left = nil, right = nil}) ->
    [];
values(#node{value = Value, left = Left, right = Right}) ->
    [Value|values(Left)] ++ values(Right).

map(_F, #node{value = nil, left = nil, right = nil}) ->
    empty();
map(F, #node{value = Value, left = Left, right = Right}) ->
    #node{value = F(Value), left = map(F, Left), right = map(F, Right)}.

fold(_F, Acc, #node{value = nil, left = nil, right = nil}) ->
    Acc;
fold(F, Acc, #node{value = Value, left = Left, right = Right}) ->
    fold(F, F(fold(F, Acc, Left), Value), Right).

values2(Tree) ->
    fold(fun(Acc, Cur) -> Acc ++ Cur end, [], map(fun(X) -> [X] end, Tree)).

% Private API

empty() ->
    #node{}.
