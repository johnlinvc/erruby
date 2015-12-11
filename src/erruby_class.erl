-module(erruby_class).
-export([new_class/0, new_class/1]).

%TODO add name parameter
new_class() ->
  erruby_object:start_link(erruby_object:class_class()).

new_class(SuperClass) ->
  Properties = #{superclass => SuperClass},
  erruby_object:start_link(erruby_object:class_class(), Properties).
