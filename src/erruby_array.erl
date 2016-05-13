-module(erruby_array).
-export([install_array_classes/0, new_array/2, new_array/1]).
-export([array_to_list/1, push/2]).

%TODO find a way to define module_function
install_array_classes() ->
  {ok, ArrayClass} = erruby_class:new_class(),
  'Array' = erruby_object:def_global_const('Array', ArrayClass),
  ok.

%TODO maybe use pid to find class
new_array(Env, Elements) ->
  ArrayClass = erruby_object:find_global_const('Array'),
  Properties = #{elements => Elements},
  {ok, Array} = erruby_object:new_object(ArrayClass, Properties),
  erruby_rb:return(Array, Env).

new_array(Elements) ->
  ArrayClass = erruby_object:find_global_const('Array'),
  Properties = #{elements => Elements},
  {ok, Array} = erruby_object:new_object(ArrayClass, Properties),
  Array.

%% @doc the Index is 0-based, not the 1-based of usual erlang
at(Array, Index) ->
  Properties = erruby_object:get_properties(Array),
  #{ elements := Elements} = Properties,
  list:nth(Index+1, Elements).

push(Array, Elem) ->
  Elements = array_to_list(Array),
  NewElements = [Elem | Elements],
  Properties = erruby_object:get_properties(Array),
  NewProperties = Properties#{ elements := NewElements} ,
  erruby_object:set_properties(Array, NewProperties).

array_to_list(Array) ->
  Properties = erruby_object:get_properties(Array),
  #{ elements := Elements} = Properties,
  Elements.
