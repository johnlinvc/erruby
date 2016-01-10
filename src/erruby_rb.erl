%%% TODO move general runtime api to here
-module(erruby_rb).
-export([ret_self/1,ret_val/1]).

ret_self( #{self := Self} = Env ) ->
  Env#{ret_val => Self}.

ret_val( #{ret_val := RetVal} ) -> RetVal.
