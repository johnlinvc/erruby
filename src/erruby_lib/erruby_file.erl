-module(erruby_file).
-include("../rb.hrl").
-export([install_file_classes/0]).

install_file_classes() ->
  {ok, FileClass} = erruby_class:new_class(),
  'File' = erruby_object:def_global_const('File', FileClass),
  erruby_object:def_method(FileClass, 'expand_path', fun method_expand_path/3),
  ok.

%FIXME add impl
method_expand_path(#{self := Self}=Env, Filename, Dirname) ->
  Env.
