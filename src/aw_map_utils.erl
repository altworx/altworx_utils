%%%-------------------------------------------------------------------
%%% @author Adam Kovari <adam.kovari@altworx.com>
%%% @copyright (C) 2016, Altworx s.r.o.
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(aw_map_utils).

-export([mapget/4, multimap_from_list/1, deep_merge_maps/2, deep_map_to_proplist/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%--------------------------------------------------------------------
%% @doc Map a function over the value in a Map or return default value
%% @end
%%--------------------------------------------------------------------
-spec mapget(Fun :: fun((X :: term()) -> Value :: V), Key :: term(), Map :: map(), Default :: D) -> V | D.
mapget(F, K, M, D) when is_function(F), is_map(M) ->
    case maps:find(K, M) of
        {ok, V} ->
            F(V);
        error ->
            D
    end.

%%--------------------------------------------------------------------
%% @doc Create a map from a proplists, where unique keys contain list of values
%% @end
%%--------------------------------------------------------------------
-spec multimap_from_list(list()) -> #{any() => list()}.
multimap_from_list(L) ->
    multimap_from_list(L, #{}).


%%--------------------------------------------------------------------
%% @doc Deep-merge maps, nested maps on the with the same keys in both maps on input are merged into a single map
%% @end
%%--------------------------------------------------------------------
-spec deep_merge_maps(map(), map()) -> map().
deep_merge_maps(M1, M2) when is_map(M1), is_map(M2) ->
    M = maps:merge(M1, M2),
    deep_merge_maps_(M, maps:to_list(M1)).

deep_merge_maps_(M, []) -> M;
deep_merge_maps_(M, [{K, #{} = V1} | T]) ->
    deep_merge_maps_(case maps:get(K, M, undefined) of
                         #{} = V2 ->
                             maps:update(K, deep_merge_maps(V1, V2), M);
                         _ -> M
                     end, T);
deep_merge_maps_(M, [_|T]) -> deep_merge_maps_(M, T).


%%--------------------------------------------------------------------
%% @doc Deep-convert maps into a nested proplist
%% @end
%%--------------------------------------------------------------------
-spec deep_map_to_proplist(map()) -> [{_, _}].
deep_map_to_proplist(M) when is_map(M) ->
    [ {K, deep_map_to_proplist(V)} || {K, V} <- maps:to_list(M) ];
deep_map_to_proplist(E) ->
    E.


%% Private
multimap_from_list([{K, V} | T], Acc) ->
    case maps:is_key(K, Acc) of
        true -> multimap_from_list(T, Acc#{K => [V | maps:get(K, Acc)]});
        false -> multimap_from_list(T, Acc#{K => [V]})
    end;
multimap_from_list([], Acc) ->
    Acc.


-ifdef(TEST).

multimap_from_list_1_test() ->
    ?assertEqual(#{a => [3, 2, 1], b => [4]}, multimap_from_list([{a, 1}, {a, 2}, {a, 3}, {b, 4}])).

deep_merge_maps_1_test() ->
    ?assertEqual(#{cde1 => 1.5, cde2 => 6}, deep_merge_maps(#{cde1 => 1.5}, #{cde2 => 6})).

deep_merge_maps_1_5_test() ->
    ?assertEqual(#{cde1 => 6}, deep_merge_maps(#{cde1 => 1.5}, #{cde1 => 6})).

deep_merge_maps_2_test() ->
    ?assertEqual(
       #{company => test_company, out => #{cde1 => 1.5, cde2 => 6, cde3 => 4}},
       deep_merge_maps(#{company => test_company, out => #{cde1 => 1.5}}, #{out => #{cde2 => 6, cde3 => 4}})
      ).

deep_merge_maps_3_test() ->
    ?assertEqual(#{out => #{nested => #{cde1 => 1.5, cde2 => 6}}}, deep_merge_maps(#{out => #{nested => #{cde1 => 1.5}}}, #{out => #{nested => #{cde2 => 6}}})).

deep_map_to_proplist_1_test() ->
    ?assertEqual([{foo, [{bar, [{baz, "a string"}]}]}], deep_map_to_proplist(#{foo => #{bar => #{baz => "a string"}}})).

-endif.
