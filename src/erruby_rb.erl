%%% TODO move general runtime api to here
-module(erruby_rb).
-export([ret_self/1]).

ret_self( #{self := Self} = Env ) ->
  Env#{ret_val => Self}.
