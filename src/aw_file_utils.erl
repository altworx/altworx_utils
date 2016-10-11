-module(aw_file_utils).

-export([make_temp/2, make_fifo/1]).

%%--------------------------------------------------------------------
%% @doc Create temp file or directory
%% @end
%%--------------------------------------------------------------------
-spec make_temp(file | dir, Prefix :: iolist()) -> string().
make_temp(file, Prefix) ->
    lib:nonl(os:cmd( binary_to_list(iolist_to_binary(["mktemp -t ", Prefix]) )));
make_temp(dir, Prefix) ->
    lib:nonl(os:cmd( binary_to_list(iolist_to_binary(["mktemp -t ", Prefix, " -d"] )))).

%%--------------------------------------------------------------------
%% @doc Create fifo file
%% @end
%%--------------------------------------------------------------------
-spec make_fifo(iolist()) -> string().
make_fifo(Prefix) ->
    Dir = make_temp(dir, Prefix),
    Path = filename:join(Dir, iolist_to_binary([Prefix, ".fifo"])),
    os:cmd( "mkfifo " ++ Path ),
    Path.
