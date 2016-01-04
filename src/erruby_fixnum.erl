-module(erruby_fixnum).
-export([install_fixnum_class/0, new_fixnum/2]).

%%
%% @TODO inherent from integer & numeric
install_fixnum_class() ->
  {ok, FixnumClass} = erruby_class:new_class(),
  'Fixnum' = erruby_object:def_global_const('Fixnum', FixnumClass),
  erruby_object:def_method(FixnumClass, to_s, fun method_to_s/1),
  erruby_object:def_method(FixnumClass, '-@', fun method_neg/1),
  erruby_object:def_method(FixnumClass, '+', fun method_add/2),
  erruby_object:def_method(FixnumClass, '-', fun method_minus/2),
  erruby_object:def_method(FixnumClass, '*', fun method_multiplication/2),
  erruby_object:def_method(FixnumClass, '/', fun method_division/2),
  erruby_object:def_method(FixnumClass, '%', fun method_module/2),
  ok.

fixnum_class() ->
  erruby_object:find_global_const('Fixnum').

new_fixnum(Env, N) ->
  {ok, Obj} = erruby_object:new_object(fixnum_class(), #{val => N}),
  Env#{ret_val => Obj}.

get_val(Fixnum) ->
  #{val := Val} = erruby_object:get_properties(Fixnum),
  Val.

binary_op(#{self := Self}=Env, AnotherFixnum, Fun) ->
  Val = Fun(get_val(Self), get_val(AnotherFixnum)),
  new_fixnum(Env, Val).

method_to_s(#{self := Self}=Env) ->
  Val = get_val(Self),
  Env#{ret_val => integer_to_list(Val)}.

%% TODO handle case where the other is not Fixnum
method_add(Env, AnotherFixnum) ->
  binary_op(Env, AnotherFixnum, fun (A,B) -> A+B end).

method_minus(Env, AnotherFixnum) ->
  binary_op(Env, AnotherFixnum, fun (A,B) -> A-B end).

method_multiplication(Env, AnotherFixnum) ->
  binary_op(Env, AnotherFixnum, fun (A,B) -> A*B end).

method_division(Env, AnotherFixnum) ->
  binary_op(Env, AnotherFixnum, fun (A,B) -> trunc(A/B) end).

method_neg(#{self := Self}=Env) ->
  Val = - get_val(Self),
  new_fixnum(Env, Val).

method_module(Env, AnotherFixnum) ->
  binary_op(Env, AnotherFixnum, fun (A,B) -> A rem B end).
