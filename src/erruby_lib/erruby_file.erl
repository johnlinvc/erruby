-module(erruby_file).
-include("../rb.hrl").
-export([install_file_classes/0]).

install_file_classes() ->
  {ok, FileClass} = erruby_class:new_class(),
  'File' = erruby_object:def_global_const('File', FileClass),
  erruby_object:def_singleton_method(FileClass, 'expand_path', fun method_expand_path/3),
  ok.

%FIXME add impl
method_expand_path(Env, Filename, RelativeDirOrFileName) ->
  % impl ref https://github.com/mochi/mochiweb/blob/b7f3693a9008de6d31a67174f7184fe24093a1b4/src/mochiweb_util.erl#L72
  {ok, Cwd} = file:get_cwd(),
  DirOrFileName = filename:absname_join(Cwd, RelativeDirOrFileName),
  DirName = filename:dirname(DirOrFileName),
  ExpanedPath = filename:absname_join(DirName, Filename),
  FlattenedPath = filename:flatten(ExpanedPath),
  erruby_vm:new_string(FlattenedPath, Env).
