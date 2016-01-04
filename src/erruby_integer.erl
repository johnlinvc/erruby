-module(erruby_integer).
-export([install_integer_class/0]).

install_integer_class() ->
  {ok, IntegerClass} = erruby_class:new_class(),
  'Integer' = erruby_object:def_global_const('Integer', IntegerClass),
  erruby_object:def_method(IntegerClass, to_i, fun method_to_i/1),
  erruby_object:def_method(IntegerClass, to_int, fun method_to_i/1),
  erruby_object:def_method(IntegerClass, floor, fun method_to_i/1),
  erruby_object:def_method(IntegerClass, ceil, fun method_to_i/1),
  erruby_object:def_method(IntegerClass, truncate, fun method_to_i/1),
  ok.

method_to_i(#{self := Self}=Env) -> Env#{ret_val => Self}.
