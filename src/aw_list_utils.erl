%%%-------------------------------------------------------------------
%%% @author Adam Kovari <adam.kovari@altworx.com>
%%% @copyright (C) 2016, Altworx s.r.o.
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(aw_list_utils).

-export([mapfirst/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%--------------------------------------------------------------------
%% @doc Map a function over the first element matching a filter
%% @end
%%--------------------------------------------------------------------
-spec mapfirst(MapF :: fun((E) -> Res), FilterF :: fun((E) -> boolean()), L :: [E]) -> Res | nomatch.
mapfirst(MapF, FilterF, [H | T]) ->
    case FilterF(H) of
        true -> MapF(H);
        false -> mapfirst(MapF, FilterF, T)
    end;
mapfirst(_MapF, _FilterF, []) ->
    nomatch.
