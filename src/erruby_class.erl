-module(erruby_class).
-export([new_class/0, new_class/1, install_class_class_methods/0, init_class_class/0]).

%TODO return self when calling method_class on self
%TODO add name parameter
%TODO should call initialize method when new

new_class() ->
  erruby_object:start_link(class_class()).

new_class(SuperClass) ->
  Properties = #{superclass => SuperClass},
  erruby_object:start_link(class_class(), Properties).

install_class_class_methods() ->
  erruby_object:def_method(class_class(), 'new', fun method_new/1),
  ok.

%FIXME new a real class
method_new(#{self := Klass}=Env) ->
  {ok, NewObject} = erruby_object:start_link(Klass),
  Env#{ret_val => NewObject}.

%TODO lazy init
init_class_class() ->
  Properties = #{superclass => erruby_object:object_class()},
  {ok, Pid} = erruby_object:new_object_with_pid_symbol(erruby_class_class, erruby_object:object_class()),
  ok = install_class_class_methods(),
  {ok, Pid}.

class_class() ->
  whereis(erruby_class_class).

