-module(erruby_boolean).
-export([install_boolean_classes/0,new_true/1,new_false/1,true_instance/0,false_instance/0]).
%TODO register the True & False class in Const

install_boolean_classes() ->
  {ok, TrueClass} = erruby_class:new_class(),
  {ok, FalseClass} = erruby_class:new_class(),
  'TrueClass' = erruby_object:def_global_const('TrueClass', TrueClass),
  'FalseClass' = erruby_object:def_global_const('FalseClass', FalseClass),
  install_method(TrueClass, FalseClass, '!', fun method_not/1),
  install_method(TrueClass, FalseClass, '&', fun method_and/2),
  install_method(TrueClass, FalseClass, '^', fun method_xor/2),
  install_method(TrueClass, FalseClass, '|', fun method_or/2),
  erruby_object:def_method(TrueClass, to_s, fun method_true_to_s/1),
  erruby_object:def_method(FalseClass, to_s, fun method_false_to_s/1),
  erruby_object:def_method(TrueClass, inspect, fun method_true_to_s/1),
  erruby_object:def_method(FalseClass, inspect, fun method_false_to_s/1),
  erruby_object:new_object_with_pid_symbol(erruby_boolean_true, TrueClass),
  erruby_object:new_object_with_pid_symbol(erruby_boolean_false, FalseClass),
  ok.

install_method(TC, FC, Name, Func) ->
  erruby_object:def_method(TC, Name, Func),
  erruby_object:def_method(FC, Name, Func).

new_true(Env) -> erruby_rb:return(true_instance(), Env).
new_false(Env) -> erruby_rb:return(false_instance(), Env).

true_instance() ->
  whereis(erruby_boolean_true).

false_instance() ->
  whereis(erruby_boolean_false).

method_not(#{self := Self} = Env) ->
  True = true_instance(),
  False = false_instance(),
  RetVal = case Self of
    True -> False;
    False -> True
  end,
  erruby_rb:return(RetVal, Env).

method_and(#{self := Self} = Env, Object) ->
  Another = object_to_boolean(Object),
  RetVal = and_op(Self, Another),
  erruby_rb:return(RetVal, Env).

object_to_boolean(Object) ->
  NilObject = erruby_nil:nil_instance(),
  False = false_instance(),
  case Object of
    NilObject -> false_instance();
    False -> false_instance();
    _ -> true_instance()
  end.


and_op(B1,B2) ->
  True = true_instance(),
  False = false_instance(),
  case B1 of
    True -> B2;
    False -> False
  end.

or_op(B1,B2) ->
  True = true_instance(),
  False = false_instance(),
  case B1 of
    True -> True;
    False -> B2
  end.

not_op(Boolean) ->
  True = true_instance(),
  False = false_instance(),
  case Boolean of
    True -> False;
    False -> True
  end.

method_or(#{self := Self} = Env, Object) ->
  Another = object_to_boolean(Object),
  RetVal = or_op(Self, Another),
  erruby_rb:return(RetVal, Env).

method_xor(#{self := Self} = Env, Object) ->
  Another = object_to_boolean(Object),
  NotAandB = and_op(not_op(Self),Another),
  AandNotB = and_op(Self, not_op(Another)),
  RetVal = or_op(NotAandB, AandNotB),
  erruby_rb:return(RetVal, Env).

method_true_to_s(Env) ->
  erruby_vm:new_string("true", Env).

method_false_to_s(Env) ->
  erruby_vm:new_string("false",Env).
