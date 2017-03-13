%%%-------------------------------------------------------------------
%%% @author Adam Kovari <adam.kovari@altworx.com>
%%% @copyright (C) 2017, Altworx s.r.o.
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(aw_async_utils).

-export([execute_all/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%%--------------------------------------------------------------------
%% @doc Asynchronously execute tasks and return a list of all results.
%% Order of the results is undefined.
%% @end
%%--------------------------------------------------------------------
-spec execute_all(fun (() -> T), timeout()) -> [T].
execute_all(Funs, Timeout) when is_list(Funs) ->
    Self = self(),
    Refs = [ begin
                 Ref = make_ref(),
                 {Pid, ProcessRef} = spawn_monitor(fun () -> Self ! {Ref, Fun()} end),
                 {Pid, ProcessRef, Ref}
             end || Fun <- Funs ],
    collect_results(Refs, Timeout, []).


%% Private
collect_results([], _Timeout, Acc) -> Acc;
collect_results(Refs, Timeout, Acc) when is_list(Refs) ->
    receive
        {Ref, Res} ->
            case lists:keytake(Ref, 3, Refs) of
                false ->
                    collect_results(Refs, Timeout, Acc); %% Ignore this message
                {value, {_Pid, _MonitorRef, Ref}, NewRefs} ->
                    collect_results(NewRefs, Timeout, [Res | Acc])
            end;

        {'DOWN', MonitorRef, _Type, _Object, Info} ->
            case lists:keytake(MonitorRef, 2, Refs) of
                false ->
                    collect_results(Refs, Timeout, Acc); %% Ignore this message
                {value, {_Pid, MonitorRef, _Ref}, NewRefs} ->
                    collect_results(NewRefs, Timeout, [Info | Acc])
            end
    after
        Timeout ->
            Acc
    end.


-ifdef(TEST).

collect_results_1_test() ->
    Res =
        execute_all([ fun () -> 1 end
                    , fun () -> 2 end
                    ], infinity),

    ?assert(lists:member(1, Res)),
    ?assert(lists:member(2, Res)),
    ?assertEqual(2, length(Res)).

collect_results_2_test() ->
    Res =
        execute_all([ fun () -> exit(error) end
                    , fun () -> 2 end
                    ], infinity),

    ?assert(lists:member(2, Res)),
    ?assert(lists:member(error, Res)),
    ?assertEqual(2, length(Res)).

-endif.
