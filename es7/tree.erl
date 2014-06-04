-module(tree).
-export([empty/0, singleton/1, insert/2, elem/2, sum/1, values/1, map/2, fold/3, values2/1]).
-record(tree, {value = nil :: any(),
               left = nil :: tree() | nil,
               right = nil :: tree() | nil}).
-type tree() :: #tree{}.

% Public API

-spec empty() -> tree().
empty() ->
    #tree{}.

-spec singleton(any()) -> tree().
singleton(Value) ->
    #tree{value = Value, left = empty(), right = empty()}.

% Nums = [8,6,4,1,7,3,5].
% lists:foldr(fun tree:insert/2, tree:empty(), Nums).
-spec insert(any(), tree()) -> tree().
insert(Value, #tree{value = nil, left = nil, right = nil}) ->
    singleton(Value);
insert(Value, #tree{value = CurValue, left = _Left, right = _Right} = Tree) when Value == CurValue ->
    Tree;
insert(Value, #tree{value = CurValue, left = Left, right = _Right} = Tree) when Value < CurValue ->
    NewLeft = insert(Value, Left),
    Tree#tree{left = NewLeft};
insert(Value, #tree{value = _CurValue, left = _Left, right = Right} = Tree) ->
    NewRight = insert(Value, Right),
    Tree#tree{right = NewRight}.

-spec elem(any(), tree()) -> boolean().
elem(_Searched, #tree{value = nil, left = nil, right = nil}) ->
    false;
elem(Searched, #tree{value = Value, left = _Left, right = _Right}) when Searched == Value ->
    true;
elem(Searched, #tree{value = Value, left = Left, right = _Right}) when Searched < Value ->
    elem(Searched, Left);
elem(Searched, #tree{value = _Value, left = _Left, right = Right}) ->
    elem(Searched, Right).

-spec sum(tree()) -> integer().
sum(#tree{value = nil, left = nil, right = nil}) ->
    0;
sum(#tree{value = Value, left = Left, right = Right}) ->
    Value + sum(Left) + sum(Right).

-spec values(tree()) -> [any()].
values(#tree{value = nil, left = nil, right = nil}) ->
    [];
values(#tree{value = Value, left = Left, right = Right}) ->
    [Value|values(Left)] ++ values(Right).

-spec map(fun((any()) -> any()), tree()) -> tree().
map(_F, #tree{value = nil, left = nil, right = nil}) ->
    empty();
map(F, #tree{value = Value, left = Left, right = Right}) ->
    #tree{value = F(Value), left = map(F, Left), right = map(F, Right)}.

-spec fold(fun((any(), any()) -> any()), any(), tree()) -> any().
fold(_F, Acc, #tree{value = nil, left = nil, right = nil}) ->
    Acc;
fold(F, Acc, #tree{value = Value, left = Left, right = Right}) ->
    fold(F, F(fold(F, Acc, Left), Value), Right).

-spec values2(tree()) -> [any()].
values2(Tree) ->
    fold(fun(Acc, Cur) -> Acc ++ Cur end, [], map(fun(X) -> [X] end, Tree)).

% Private API
