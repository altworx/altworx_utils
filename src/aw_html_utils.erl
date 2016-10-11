%%%-------------------------------------------------------------------
%%% @author Adam Kovari <adam.kovari@altworx.com>
%%% @copyright (C) 2016, Altworx s.r.o.
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(aw_html_utils).
-author("akovari").

%% API
-export([htmlize/1, htmlize/2]).

%%--------------------------------------------------------------------
%% @doc Escape HTML string
%% @end
%%--------------------------------------------------------------------
htmlize([]) -> [];
htmlize([$<|T]) -> "&lt;" ++ htmlize(T);
htmlize([$>|T]) -> "&gt;" ++ htmlize(T);
htmlize([$&|T]) -> "&amp;" ++ htmlize(T);
htmlize([H|T]) when is_integer(H) -> [H|htmlize(T)];
htmlize([H|T]) -> [htmlize(H)|htmlize(T)];
htmlize(Bin) when is_binary(Bin) ->
    htmlize(Bin, <<>>);
htmlize(_) -> error(badarg).

htmlize(<<$<, Rest/bytes>>, Acc) -> htmlize(Rest, <<Acc/bytes, "&lt;">>);
htmlize(<<$>, Rest/bytes>>, Acc) -> htmlize(Rest, <<Acc/bytes, "&gt;">>);
htmlize(<<$&, Rest/bytes>>, Acc) -> htmlize(Rest, <<Acc/bytes, "&amp;">>);
htmlize(<<C/utf8, Rest/bytes>>, Acc) -> htmlize(Rest, <<Acc/bytes, C/utf8>>);
htmlize(_, Acc) -> Acc.
