%%%-------------------------------------------------------------------
%%% @author Adam Kovari <adam.kovari@altworx.com>
%%% @copyright (C) 2016, Altworx s.r.o.
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(aw_date_utils).

-export([erlang_ts_to_int/1, int_to_erlang_ts/1]).

%%--------------------------------------------------------------------
%% @doc Convert erlang timestamp to integer
%% @end
%%--------------------------------------------------------------------
-spec erlang_ts_to_int(erlang:timestamp()) -> integer().
erlang_ts_to_int({Mega, Sec, _Micro}) ->
    Mega * 1000000 + Sec.


%%--------------------------------------------------------------------
%% @doc Convert integer to erlang timestamp
%% @end
%%--------------------------------------------------------------------
-spec int_to_erlang_ts(integer()) -> erlang:timestamp().
int_to_erlang_ts(Timestamp) when is_number(Timestamp) ->
    {Timestamp div 1000000, Timestamp rem 1000000, 0}.
