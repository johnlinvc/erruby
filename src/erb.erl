-module(erb).
-export([find_or_init_class/2]).

find_or_init_class(Name, InitFun) ->
  case whereis(Name) of
    undefined -> InitFun();
    Pid -> {ok, Pid}
  end.
