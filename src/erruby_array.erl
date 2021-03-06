-module(erruby_array).
-include("rb.hrl").
-export([install_array_classes/0, new_array/2, new_array/1]).
-export([array_to_list/1, push/2]).

%TODO find a way to define module_function
install_array_classes() ->
  {ok, ArrayClass} = erruby_class:new_class(),
  'Array' = erruby_object:def_global_const('Array', ArrayClass),
  erruby_object:def_method(ArrayClass, map, fun method_map/1),
  erruby_object:def_method(ArrayClass, pmap, fun method_pmap/1),
  erruby_object:def_method(ArrayClass, '*' , fun method_multiplication/2),
  erruby_object:def_method(ArrayClass, '+', fun method_plus/2),
  erruby_object:def_method(ArrayClass, 'concat', fun method_concat/2),
  erruby_object:def_method(ArrayClass, 'at' , fun method_at/2),
  erruby_object:def_method(ArrayClass, 'first' , fun method_first/1),
  erruby_object:def_method(ArrayClass, 'last' , fun method_last/1),
  erruby_object:def_method(ArrayClass, 'empty?', fun method_empty_q/1),
  erruby_object:def_method(ArrayClass, 'length', fun method_length/1),
  erruby_object:def_method(ArrayClass, 'size', fun method_length/1),
  erruby_object:def_method(ArrayClass, 'push' , fun method_push/2),
  erruby_object:def_method(ArrayClass, '<<' , fun method_push/2),
  erruby_object:def_method(ArrayClass, 'unshift' , fun method_unshift/2),
  erruby_object:def_method(ArrayClass, 'shift' , fun method_shift/1),
  erruby_object:def_method(ArrayClass, 'drop' , fun method_drop/2),
  ok.

drop_elements(_List, Count) when Count =< 0 ->
  _List;
drop_elements([Head | Tail], Count) ->
  drop_elements(Tail, Count - 1).

method_drop(#{self := Self}=Env, IntObj) ->
  Int = erruby_fixnum:fix_to_int(IntObj),
  List = array_to_list(Self),
  if
    length(List) < Int ->
      new_array(Env, []);
    true ->
      ResultList = drop_elements(List, Int),
      new_array(Env, ResultList)
  end.

method_map(#{self := Self}=Env) ->
  List = array_to_list(Self),
  FoldFun = fun(X, EnvAcc) -> erruby_vm:yield(EnvAcc, [X]) end,
  Envs = erruby_vm:scanl(FoldFun, Env, List),
  Results = lists:map(fun erruby_rb:ret_val/1, Envs),
  erruby_rb:return(Results, lists:last(Envs)).

repeat_list(_List, Count) when Count =< 0 ->
  new_array([]);
repeat_list(List, 1) ->
  List;
repeat_list(List, Count) ->
  lists:append(List, repeat_list(List, Count-1)).

method_multiplication(#{self := Self}=Env, IntObj) ->
  Int = erruby_fixnum:fix_to_int(IntObj),
  List = array_to_list(Self),
  ResultList = repeat_list(List, Int),
  new_array(Env, ResultList).

method_plus(#{self := Self}=Env, Another) ->
  Elements = array_to_list(Self),
  AnotherElements = array_to_list(Another),
  NewElements = Elements ++ AnotherElements,
  new_array(Env, NewElements).

method_concat(#{self := Self}=Env, Another) ->
  Elements = array_to_list(Self),
  AnotherElements = array_to_list(Another),
  NewElements = Elements ++ AnotherElements,
  Properties = erruby_object:get_properties(Self),
  NewProperties = Properties#{ elements := NewElements},
  erruby_object:set_properties(Self, NewProperties),
  erruby_rb:return(Self, Env).

method_pmap(#{self := Self}=Env) ->
  List = array_to_list(Self),
  MapFun = fun(X) -> erruby_vm:yield(Env, [X]) end,
  Envs = plists:map(MapFun, List, {processes, erlang:system_info(schedulers_online)}),
  Results = lists:map(fun erruby_rb:ret_val/1, Envs),
  erruby_rb:return(Results, lists:last(Envs)).

method_at(#{self := Self}=Env, IntObj) ->
  Int = erruby_fixnum:fix_to_int(IntObj),
  erruby_rb:return(at(Self, Int), Env).

method_first(#{self := Self}=Env) ->
  List = array_to_list(Self),
  [ Head | _Tail ] = List,
  erruby_rb:return(Head, Env).

method_last(#{self := Self}=Env) ->
  List = array_to_list(Self),
  erruby_rb:return(lists:last(List), Env).

method_empty_q(#{self := Self}=Env) ->
  List = array_to_list(Self),
  case  length(List) < 1 of
    true -> erruby_boolean:new_true(Env);
    false -> erruby_boolean:new_false(Env)
  end.
method_length(#{self := Self}=Env) ->
  List = array_to_list(Self),
  erruby_fixnum:new_fixnum(Env, length(List)).

method_push(#{self := Self}=Env, Append) ->
  push(Self, Append),
  erruby_rb:return(Self, Env).

method_unshift(#{self := Self}=Env, Head) ->
  Elements = array_to_list(Self),
  NewElements = [Head | Elements],
  Properties = erruby_object:get_properties(Self),
  NewProperties = Properties#{ elements := NewElements} ,
  erruby_object:set_properties(Self, NewProperties),
  erruby_rb:return(Self, Env).

method_shift(#{self := Self}=Env) ->
  Elements = array_to_list(Self),
  [Head | Rest] = Elements,
  Properties = erruby_object:get_properties(Self),
  NewProperties = Properties#{ elements := Rest} ,
  erruby_object:set_properties(Self, NewProperties),
  erruby_rb:return(Head, Env).

%TODO maybe use pid to find class
new_array(Env, Elements) ->
  erruby_rb:return(new_array(Elements), Env).

new_array(Elements) ->
  ArrayClass = erruby_object:find_global_const('Array'),
  Properties = #{elements => Elements},
  {ok, Array} = erruby_object:new_object(ArrayClass, Properties),
  Array.

%% @doc the Index is 0-based, not the 1-based of usual erlang
at(Array, Index) ->
  Properties = erruby_object:get_properties(Array),
  #{ elements := Elements} = Properties,
  lists:nth(Index+1, Elements).

push(Array, Elem) ->
  Elements = array_to_list(Array),
  NewElements = Elements ++ [Elem],
  Properties = erruby_object:get_properties(Array),
  NewProperties = Properties#{ elements := NewElements} ,
  erruby_object:set_properties(Array, NewProperties).

array_to_list(Array) ->
  Properties = erruby_object:get_properties(Array),
  #{ elements := Elements} = Properties,
  Elements.
