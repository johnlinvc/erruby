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
  erruby_object:def_method(IntegerClass, numerator, fun method_to_i/1),
  erruby_object:def_method(IntegerClass, ord, fun method_to_i/1),
  erruby_object:def_method(IntegerClass, denominator, fun method_denominator/1),
  erruby_object:def_method(IntegerClass, 'even?', fun method_even_q/1),
  erruby_object:def_method(IntegerClass, 'odd?', fun method_odd_q/1),
  erruby_object:def_method(IntegerClass, 'gcd', fun method_gcd/2),
  erruby_object:def_method(IntegerClass, 'lcm', fun method_lcm/2),
  erruby_object:def_method(IntegerClass, 'integer?', fun method_integer_q/1),
  erruby_object:def_method(IntegerClass, 'succ', fun method_succ/1),
  erruby_object:def_method(IntegerClass, 'next', fun method_succ/1),
  erruby_object:def_method(IntegerClass, 'times', fun method_times/1),
  erruby_object:def_method(IntegerClass, 'upto', fun method_upto/2),
  erruby_object:def_method(IntegerClass, 'downto', fun method_downto/2),
  ok.

method_to_i(#{self := Self}=Env) -> Env#{ret_val => Self}.

method_denominator(Env) ->
  erruby_fixnum:new_fixnum(Env, 1).

%%TODO handle Bignum
method_even_q(#{self := Self}=Env) ->
  Int = erruby_fixnum:fix_to_int(Self),
  case Int rem 2 of
    0 -> erruby_boolean:new_true(Env);
    1 -> erruby_boolean:new_false(Env)
  end.
method_odd_q(#{self := Self}=Env) ->
  Int = erruby_fixnum:fix_to_int(Self),
  case Int rem 2 of
    1 -> erruby_boolean:new_true(Env);
    0 -> erruby_boolean:new_false(Env)
  end.

%%TODO use binary gcd algo instead
%%TODO move this to rational
gcd(X,Y) when X < Y -> gcd(Y,X);
gcd(X,Y) when Y =:= 0 -> X;
gcd(X,Y) -> gcd(Y, X rem Y).

method_gcd(#{self := Self}=Env, AnotherInt) ->
  X = abs(erruby_fixnum:fix_to_int(Self)),
  Y = abs(erruby_fixnum:fix_to_int(AnotherInt)),
  case min(X,Y) of
    0 -> erruby_fixnum:new_fixnum(Env, max(X,Y));
    _ -> erruby_fixnum:new_fixnum(Env, gcd(X,Y))
  end.

lcm(0,_Y) -> 0;
lcm(_X,0) -> 0;
lcm(X,Y) -> trunc(X / gcd(X,Y) * Y).

method_lcm(#{self := Self}=Env, AnotherInt) ->
  X = abs(erruby_fixnum:fix_to_int(Self)),
  Y = abs(erruby_fixnum:fix_to_int(AnotherInt)),
  case min(X,Y) of
    0 -> erruby_fixnum:new_fixnum(Env, 0);
    _ -> erruby_fixnum:new_fixnum(Env, lcm(X,Y))
  end.

method_integer_q(Env) ->
  erruby_boolean:new_true(Env).

method_succ(#{self := Self}=Env) ->
  erruby_fixnum:new_fixnum(Env, erruby_fixnum:fix_to_int(Self)+1).


yield_in_range(#{self := Self} = Env,Range) ->
  FoldFun = fun (X, EnvAcc) ->
                IntEnv = erruby_fixnum:new_fixnum(EnvAcc, X),
                FixInt = erruby_rb:ret_val(IntEnv),
                erruby_vm:yield(IntEnv, [FixInt]) end,
  LastEnv = lists:foldl(FoldFun, Env, Range),
  LastEnv#{ret_val => Self}.

times_range(X) when X =< 0 -> [];
times_range(X) -> lists:seq(0, X-1).

%%TODO handle empty block case
%%TODO handle Bigdecimal
method_times(#{self := Self}=Env) ->
  Int = erruby_fixnum:fix_to_int(Self),
  Range = times_range(Int),
  yield_in_range(Env,Range).

upto_range(Start,End) when Start > End -> [];
upto_range(Start,End) -> lists:seq(Start,End).

method_upto(#{self := Self}=Env, AnotherInteger) ->
  Int = erruby_fixnum:fix_to_int(Self),
  AnotherInt = erruby_fixnum:fix_to_int(AnotherInteger),
  Range = upto_range(Int,AnotherInt),
  yield_in_range(Env,Range).

downto_range(Start,End) -> lists:reverse(upto_range(End,Start)).

method_downto(#{self := Self}=Env, AnotherInteger) ->
  Int = erruby_fixnum:fix_to_int(Self),
  AnotherInt = erruby_fixnum:fix_to_int(AnotherInteger),
  Range = downto_range(Int,AnotherInt),
  yield_in_range(Env,Range).
