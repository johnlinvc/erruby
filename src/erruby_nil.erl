-module(erruby_nil).
-export([new_nil/1, install_nil_class/0]).

install_nil_class() ->
  {ok, NilClass} = erruby_class:new_class(),
  erruby_object:def_method(NilClass, '&', fun method_and/2),
  erruby_object:new_object_with_pid_symbol(erruby_nil, NilClass),
  ok.
new_nil(Env) -> Env#{ret_val => whereis(erruby_nil)}.

method_and(Env, _Other) -> erruby_boolean:new_false(Env).
