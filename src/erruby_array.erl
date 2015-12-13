-module(erruby_array).
-export([install_array_classes/0, new_array/2]).

install_array_classes() ->
  {ok, ArrayClass} = erruby_class:new_class(),
  'Array' = erruby_object:def_global_const('Array', ArrayClass),
  ok.

%TODO maybe use pid to find class
new_array(Env, Elements) ->
  ArrayClass = erruby_object:find_global_const('Array'),
  Properties = #{elements => Elements},
  {ok, Array} = erruby_object:new_object(ArrayClass, Properties),
  Env#{ret_val => Array}.
