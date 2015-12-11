-module(erruby_boolean).
-export([install_boolean_classes/0,new_true/1,new_false/1]).

install_boolean_classes() ->
  {ok, TrueClass} = erruby_object:new_class(),
  {ok, FalseClass} = erruby_object:new_class(),
  install_method(TrueClass, FalseClass, '!', fun method_not/1),
  install_method(TrueClass, FalseClass, '==', fun method_eq/2),
  install_method(TrueClass, FalseClass, '&', fun method_and/2),
  erruby_object:new_object_with_pid_symbol(erruby_boolean_true, TrueClass),
  erruby_object:new_object_with_pid_symbol(erruby_boolean_false, FalseClass),
  ok.

install_method(TC, FC, Name, Func) ->
  erruby_object:def_method(TC, Name, Func),
  erruby_object:def_method(FC, Name, Func).

new_true(Env) -> Env#{ret_val => true_pid()}.
new_false(Env) -> Env#{ret_val => false_pid()}.

true_pid() ->
  whereis(erruby_boolean_true).

false_pid() ->
  whereis(erruby_boolean_false).

method_not(#{self := Self} = Env) ->
  True = true_pid(),
  False = false_pid(),
  RetVal = case Self of
    True -> False;
    False -> True
  end,
  Env#{ret_val => RetVal}.

%TODO handle nil
method_and(#{self := Self} = Env, Object) ->
  False = false_pid(),
  case Object of
    False -> new_false(Env);
    _ -> Env#{ret_val := Self}
  end.


%TODO remove this & use the one in object
method_eq(#{self := Self}=Env, Object) ->
  case Object of
    Self -> new_true(Env);
    _ -> new_false(Env)
  end.
