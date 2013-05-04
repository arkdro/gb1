-module(gb1).
-compile(export_all).

-include_lib("eqc/include/eqc.hrl").

gb_next(K, {_, T}) ->
    gb_next_1(K, T).

gb_next_1(K, {K1, _, Smaller, Bigger}) when K < K1 ->
    case gb_next_1(K, Smaller) of
	none ->
	    case gb_next_1(K, Bigger) of
		none ->
		    {value, K1};
		{value, K2} ->
		    {value, erlang:min(K1, K2)}
	    end;
	{value, _} = Res ->
	    Res
    end;
gb_next_1(K, {K1, _, _, Bigger}) when K > K1 ->
    gb_next_1(K, Bigger);
gb_next_1(K, {_, _, _, Bigger}) ->
    case Bigger of
	nil ->
	    none;
	{K1, _, Smaller, _} ->
	    case gb_next_1(K, Smaller) of
		none ->
		    {value, K1};
		{value, _} = Res ->
		    Res
	    end
    end;
gb_next_1(_, nil) ->
    none.

gb_prev(K, {_, T}) ->
    gb_prev_1(K, T).

gb_prev_1(K, {K1, _, Smaller, Bigger}) when K > K1 ->
    case gb_prev_1(K, Bigger) of
	none ->
	    case gb_prev_1(K, Smaller) of
		none ->
		    {value, K1};
		{value, K2} ->
		    {value, erlang:max(K1, K2)}
	    end;
	{value, _} = Res ->
	    Res
    end;
gb_prev_1(K, {K1, _, Smaller, _}) when K < K1 ->
    gb_prev_1(K, Smaller);
gb_prev_1(K, {_, _, Smaller, _}) ->
    case Smaller of
	nil ->
	    none;
	{K1, _, _, Bigger} ->
	    case gb_prev_1(K, Bigger) of
		none ->
		    {value, K1};
		{value, _} = Res ->
		    Res
	    end
    end;
gb_prev_1(_, nil) ->
    none.


first({_, T}) ->
    first_1(T).

first_1({K,_,nil,_}) ->
    {value, K};
first_1({_,_,Smaller,_}) ->
    first_1(Smaller);
first_1(nil) ->
    none.

last({_, T}) ->
    last_1(T).

last_1({K,_,_,nil}) ->
    {value, K};
last_1({_,_,_,Bigger}) ->
    last_1(Bigger);
last_1(nil) ->
    none.

prop_first() ->
    ?FORALL(L, list(int()),
	    begin
		{T, Sorted} = make_tree(L),
		case first(T) of
		    none -> Sorted == [];
		    {value,X} -> X == hd(Sorted)
		end
	    end).

prop_last() ->
    ?FORALL(L, list(int()),
	    begin
		{T, Sorted} = make_tree(L),
		case last(T) of
		    none -> Sorted == [];
		    {value,X} -> X == lists:last(Sorted)
		end
	    end).

prop_prev() ->
    ?FORALL(L, list(int()),
	    begin
		{T, Sorted} = make_tree(L),
		ok == all_prev(lists:reverse(Sorted), T)
	    end).

prop_next() ->
    ?FORALL(L, list(int()),
	    begin
		{T, Sorted} = make_tree(L),
		ok == all_prev(lists:reverse(Sorted), T)
	    end).

make_tree(L) ->
    T = lists:foldl(fun(X,T) ->
			    gb_trees:enter(X,1,T)
		    end, gb_trees:empty(), L),
    Sorted = [K || {K,_} <- gb_trees:to_list(T)],
    {T, Sorted}.

all_next([X], T) ->
    {X,none} = {X,gb_next(X, T)},
    ok;
all_next([A,B|Rest], T) ->
    {A,{value,B}} = {A,gb_next(A, T)},
    all_next([B|Rest], T);
all_next([], _) ->
    ok.

all_prev([X], T) ->
    {X,none} = {X,gb_prev(X, T)},
    ok;
all_prev([A,B|Rest], T) ->
    {A,{value,B}} = {A,gb_prev(A, T)},
    all_prev([B|Rest], T);
all_prev([], _) ->
    ok.

