-module(erruby_fixnum).
-export([install_fixnum_class/0, new_fixnum/2]).

%%
%% @TODO inherent from integer & numeric
install_fixnum_class() ->
  {ok, FixnumClass} = erruby_class:new_class(),
  'Fixnum' = erruby_object:def_global_const('Fixnum', FixnumClass),
  erruby_object:def_method(FixnumClass, to_s, fun method_to_s/1),
  ok.

fixnum_class() ->
  erruby_object:find_global_const('Fixnum').

new_fixnum(Env, N) ->
  {ok, Obj} = erruby_object:new_object(fixnum_class(), #{val => N}),
  Env#{ret_val => Obj}.

get_val(Fixnum) ->
  #{val := Val} = erruby_object:get_properties(Fixnum),
  Val.

method_to_s(#{self := Self}=Env) ->
  Val = get_val(Self),
  Env#{ret_val => integer_to_list(Val)}.


