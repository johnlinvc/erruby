-module(erruby_boolean).
-export([install_boolean_classes/0,new_true/1,new_false/1]).
%methods
-export([method_not/1]).

install_boolean_classes() ->
  {ok, TrueClass} = erruby_object:new_class(),
  {ok, FalseClass} = erruby_object:new_class(),
  erruby_object:def_method(TrueClass, '!', fun method_not/1),
  erruby_object:def_method(FalseClass, '!', fun method_not/1),
  erruby_object:new_object_with_pid_symbol(erruby_boolean_true, TrueClass),
  erruby_object:new_object_with_pid_symbol(erruby_boolean_false, FalseClass),
  ok.

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
