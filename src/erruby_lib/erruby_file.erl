-module(erruby_file).
-include("../rb.hrl").
-export([install_file_classes/0, expand_path/2]).

install_file_classes() ->
  {ok, FileClass} = erruby_class:new_class(),
  'File' = erruby_object:def_global_const('File', FileClass),
  erruby_object:def_singleton_method(FileClass, 'expand_path', fun method_expand_path/3),
  ok.

expand_path(Filename, RelativeDirOrFileName) ->
  {ok, Cwd} = file:get_cwd(),
  DirOrFileName = filename:absname_join(Cwd, RelativeDirOrFileName),
  ExpanedPath = filename:absname_join(DirOrFileName, Filename),
  flatten_path(ExpanedPath).

method_expand_path(Env, Filename, RelativeDirOrFileName) ->
  FlattenedPath = expand_path(Filename, RelativeDirOrFileName),
  erruby_vm:new_string(FlattenedPath, Env).

flatten_path(Path)->
  Components = filename:split(Path),
  FlattenedComponents = flatten_path_components(Components,[]),
  filename:join(FlattenedComponents).

flatten_path_components([], Acc) -> lists:reverse(Acc);
flatten_path_components([".."|T], [])->
  flatten_path_components(T, []);
flatten_path_components([".."|T], ["/"])->
  flatten_path_components(T, ["/"]);
flatten_path_components([".."|T], [_H|Acc])->
  flatten_path_components(T, Acc);
flatten_path_components([H|T], Acc)->
  flatten_path_components(T, [H|Acc]).
