%%%-------------------------------------------------------------------
%%% @author Adam Kovari <adam.kovari@altworx.com>
%%% @copyright (C) 2016, Altworx s.r.o.
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(aw_misc_utils).

-export([to_binary/1, to_list/1, binary_join/2, tuple_index_of/2, remove_common_prefix/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%--------------------------------------------------------------------
%% @doc Convert item to binary
%% @end
%%--------------------------------------------------------------------
-spec to_binary(binary() | number() | atom() | iolist()) -> binary().
to_binary(B) when is_binary(B) ->
	B;
to_binary(I) when is_integer(I) ->
    integer_to_binary(I);
to_binary(A) when is_atom(A) ->
    atom_to_binary(A, latin1);
to_binary(F) when is_float(F) ->
    float_to_binary(F);
to_binary(L) ->
    iolist_to_binary(L).

%%--------------------------------------------------------------------
%% @doc Convert item to list
%% @end
%%--------------------------------------------------------------------
-spec to_list(list() | binary() | number() | atom() | iolist()) -> list().
to_list(L) when is_list(L) ->
    L;
to_list(B) when is_binary(B) ->
    binary_to_list(B);
to_list(I) when is_integer(I) ->
    integer_to_list(I);
to_list(A) when is_atom(A) ->
    atom_to_list(A);
to_list(F) when is_float(F) ->
    float_to_list(F).

%%--------------------------------------------------------------------
%% @doc Join binaries with a separator
%% @end
%%--------------------------------------------------------------------
-spec binary_join([binary()], binary()) -> binary().
binary_join([], _Sep) ->
  <<>>;
binary_join([Part], _Sep) ->
  Part;
binary_join([Head|Tail], Sep) when is_binary(Sep) ->
    lists:foldl(fun (Value, Acc) -> <<Acc/binary, Sep/binary, Value/binary>> end, Head, Tail);
binary_join(L, Sep) when is_list(L) ->
    binary_join(L, to_binary(Sep)).


%%--------------------------------------------------------------------
%% @doc Index of an item in a tuple
%% @end
%%--------------------------------------------------------------------
-spec tuple_index_of(term(), tuple() | list()) -> non_neg_integer().
tuple_index_of(Item, Tuple) when is_tuple(Tuple) -> tuple_index_of(Item, tuple_to_list(Tuple));
tuple_index_of(Item, List) -> tuple_index_of(Item, List, 1).

tuple_index_of(_, [], _)  -> not_found;
tuple_index_of(Item, [Item|_], Index) -> Index;
tuple_index_of(Item, [_|Tl], Index) -> tuple_index_of(Item, Tl, Index+1).


%%--------------------------------------------------------------------
%% @doc Remove common prefix from two lists
%% @end
%%--------------------------------------------------------------------
-spec remove_common_prefix([], []) -> [].
remove_common_prefix(L1, L2) ->
    lists:reverse(remove_common_prefix(L1, L2, [])).

remove_common_prefix([_H | T1], [_H | T2], Acc) ->
    remove_common_prefix(T1, T2, Acc);
remove_common_prefix([], L2, Acc) ->
    lists:reverse(L2) ++ Acc;
remove_common_prefix(L1, _L2, Acc) ->
    Acc ++ lists:reverse(L1).

-ifdef(TEST).

remove_common_prefix_1_test() ->
    ?assertEqual("m ko", remove_common_prefix("ada", "adam ko")).

remove_common_prefix_2_test() ->
    ?assertEqual("m ko", remove_common_prefix("adam ko", "ada")).

-endif.
