-module(erruby_vm).
-export([eval_ast/1]).

print_ast(Ast) ->
  io:format("~p ~n",[Ast]).

print_env(Env) ->
  io:format("~p ~n",[Env]).

eval_ast({ast,type,'begin',children, Children}, Env) ->
  io:format("eval begin~n",[]),
  [ eval_ast(Ast,Env) || Ast <- Children ];

eval_ast({ast,type,send, children, Children}, Env) ->
  [_F | [Msg | Args]] = Children,
  EvaledArgs = [ eval_ast(Child, Env) || Child <- Args],
  #{ klass := Self } = Env,
  erruby_object:msg_send(Self, Msg, EvaledArgs);

eval_ast({ast, type, str, children, Children}, _Env) ->
  [SBin|_T] = Children,
  binary_to_list(SBin);

eval_ast(Ast, Env) ->
  print_ast(Ast),
  print_env(Env).

eval_ast(Ast) ->
  Env = default_env(),
  eval_ast(Ast, Env).

default_env() ->
  {ok, Kernal} = erruby_object:new_kernel(),
  #{klass => Kernal}.
