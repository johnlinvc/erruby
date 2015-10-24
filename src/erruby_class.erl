-module(erruby_class).
-export([rb_eval_class_ast/1, class_def/1]).

rb_eval_class_ast({ast,type,class,children,Children}) ->
  io:format("~p ~n",[Children]).

class_def({ast,type,class,children,Children})->
  io:format("define class ~p ~n",[Children]).
