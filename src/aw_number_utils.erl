%%%-------------------------------------------------------------------
%%% @author Adam Kovari <adam.kovari@altworx.com>
%%% @copyright (C) 2016, Altworx s.r.o.
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(aw_number_utils).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([
         percent_diff/3,
         abs_diff/3,
         binary_to_number/1,
         list_to_number/1,

         string_is_numeric/1,
         string_is_float/1,
         string_is_integer/1,
         string_integer_or_default/2,
         string_float_or_default/2
        ]).

%%--------------------------------------------------------------------
%% @doc Assert whether two numbers are apart by at most a percent value specified as the third parameter
%% @end
%%--------------------------------------------------------------------
-spec percent_diff(number(), number(), number()) -> boolean().
percent_diff(FieldValue, PrevFieldValue, ConditionBody)
  when is_number(FieldValue), is_number(ConditionBody), is_number(PrevFieldValue) ->
    1 - case FieldValue > PrevFieldValue of
            true -> (PrevFieldValue / FieldValue);
            false -> (FieldValue / PrevFieldValue)
        end > (ConditionBody / 100);
percent_diff(FieldValue, undefined, ConditionBody)
  when is_number(FieldValue), is_number(ConditionBody) ->
    true.

%%--------------------------------------------------------------------
%% @doc Assert whether two numbers are apart by at most an absolute value specified as the third parameter
%% @end
%%--------------------------------------------------------------------
-spec abs_diff(number(), number(), number()) -> boolean().
abs_diff(FieldValue, PrevFieldValue, ConditionBody)
  when is_number(FieldValue), is_number(ConditionBody), is_number(PrevFieldValue) ->
    abs(FieldValue - PrevFieldValue) > ConditionBody;
abs_diff(FieldValue, undefined, ConditionBody)
  when is_number(FieldValue), is_number(ConditionBody) ->
    true.

%%--------------------------------------------------------------------
%% @doc Convert a binary to either float or integer
%% @end
%%--------------------------------------------------------------------
-spec binary_to_number(binary()) -> number().
binary_to_number(B) ->
    try binary_to_float(B)
    catch
        error:badarg ->
            binary_to_integer(B)
    end.

%%--------------------------------------------------------------------
%% @doc Convert a list to either float or integer
%% @end
%%--------------------------------------------------------------------
-spec list_to_number(list()) -> number().
list_to_number(L) ->
    try list_to_float(L)
    catch
        error:badarg ->
            list_to_integer(L)
    end.


%%--------------------------------------------------------------------
%% @doc Assert whether a list or a binary actually represents a number (float or integer). Returns false if the input is neither a list or binary
%% @end
%%--------------------------------------------------------------------
-spec string_is_numeric(list() | binary()) -> boolean().
string_is_numeric(L) when is_list(L); is_binary(L) ->
    string_is_float(L) orelse string_is_integer(L).


%%--------------------------------------------------------------------
%% @doc Assert whether a list or a binary actually represents a float. Returns false if the input is neither a list or binary
%% @end
%%--------------------------------------------------------------------
-spec string_is_float(list() | binary()) -> boolean().
string_is_float(L) ->
    if
        is_list(L) ->
            is_float(catch erlang:list_to_float(L));
        is_binary(L) ->
            is_float(catch erlang:binary_to_float(L));
        true ->
            false
    end.


%%--------------------------------------------------------------------
%% @doc Assert whether a list or a binary actually represents a integer. Returns false if the input is neither a list or binary
%% @end
%%--------------------------------------------------------------------
-spec string_is_integer(list() | binary()) -> boolean().
string_is_integer(L) ->
    if
        is_list(L) ->
            is_integer(catch erlang:list_to_integer(L));
        is_binary(L) ->
            is_integer(catch erlang:binary_to_integer(L));
        true ->
            false
    end.


%%--------------------------------------------------------------------
%% @doc Returns an integer value of a string or default value if the input is not a string representing an integer
%% @end
%%--------------------------------------------------------------------
-spec string_integer_or_default(list() | binary(), Default) -> integer() | Default.
string_integer_or_default(I, Default) ->
    case string_is_integer(I) of
        true ->
            I;
        false ->
            Default
    end.


%%--------------------------------------------------------------------
%% @doc Returns a float value of a string or default value if the input is not a string representing a float
%% @end
%%--------------------------------------------------------------------
-spec string_float_or_default(list() | binary(), Default) -> float() | Default.
string_float_or_default(F, Default) ->
    case string_is_float(F) of
        true ->
            F;
        false ->
            Default
    end.


-ifdef(TEST).

percent_diff_1_test() ->
    ?assertEqual(false, percent_diff(50, 47, 10)).

percent_diff_2_test() ->
    ?assertEqual(true, percent_diff(50, 44, 10)).

percent_diff_3_test() ->
    ?assertEqual(false, percent_diff(50, 53, 10)).

percent_diff_4_test() ->
    ?assertEqual(true, percent_diff(50, 56, 10)).

abs_diff_1_test() ->
    ?assertEqual(false, abs_diff(50, 47, 5)).

abs_diff_2_test() ->
    ?assertEqual(true, abs_diff(50, 44, 5)).

abs_diff_3_test() ->
    ?assertEqual(false, abs_diff(50, 54, 5)).

abs_diff_4_test() ->
    ?assertEqual(true, abs_diff(50, 56, 5)).

string_is_float_1_test() ->
    ?assertEqual(false, string_is_float("q")).

string_is_float_2_test() ->
    ?assertEqual(false, string_is_float(<<"q">>)).

string_is_float_3_test() ->
    ?assertEqual(true, string_is_float("1.3")).

string_is_float_4_test() ->
    ?assertEqual(true, string_is_float(<<"1.5">>)).

string_is_float_5_test() ->
    ?assertEqual(false, string_is_float(<<"1">>)).

string_is_integer_1_test() ->
    ?assertEqual(false, string_is_integer("q")).

string_is_integer_2_test() ->
    ?assertEqual(false, string_is_integer(<<"q">>)).

string_is_integer_3_test() ->
    ?assertEqual(true, string_is_integer("1")).

string_is_integer_4_test() ->
    ?assertEqual(true, string_is_integer(<<"1">>)).

string_is_integer_5_test() ->
    ?assertEqual(false, string_is_integer(<<"1.2">>)).

string_is_number_1_test() ->
    ?assertEqual(true, string_is_numeric(<<"1.2">>)).

string_is_number_2_test() ->
    ?assertEqual(true, string_is_numeric("2")).

-endif.
