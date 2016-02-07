-module(erruby_nil).
-export([new_nil/1, install_nil_class/0, nil_instance/0]).

install_nil_class() ->
  {ok, NilClass} = erruby_class:new_class(),
  'NilClass' = erruby_object:def_global_const('NilClass', NilClass),
  erruby_object:def_method(NilClass, '&', fun method_and/2),
  erruby_object:def_method(NilClass, '^', fun method_xor/2),
  erruby_object:def_method(NilClass, '|', fun method_xor/2),
  erruby_object:def_method(NilClass, inspect, fun method_inspect/1),
  erruby_object:def_method(NilClass, 'nil?', fun 'method_nil_q'/1),
  erruby_object:def_method(NilClass, 'to_s', fun 'method_to_s'/1),
  erruby_object:new_object_with_pid_symbol(erruby_nil, NilClass),
  ok.

new_nil(Env) ->
  erruby_rb:return(nil_instance(), Env).

nil_instance() -> whereis(erruby_nil).

method_and(Env, _Obj) -> erruby_boolean:new_false(Env).

method_xor(Env, Obj) ->
  Nil = nil_instance(),
  False = erruby_boolean:false_instance(),
  case Obj of
    Nil -> erruby_boolean:new_false(Env);
    False -> erruby_boolean:new_false(Env);
    _ -> erruby_boolean:new_true(Env)
  end.

method_inspect(Env) -> erruby_vm:new_string("nil",Env).

method_nil_q(Env) -> erruby_boolean:new_true(Env).

method_to_s(Env) -> erruby_vm:new_string("",Env).
