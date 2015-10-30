-module(erruby_vm).
-export([eval_ast/1]).

print_ast(Ast) ->
  erruby_debug:debug_1("Ast: ~p ~n",[Ast]).

print_env(Env) ->
  erruby_debug:debug_1("Env: ~p ~n",[Env]).

eval_ast({ast,type,'begin',children, Children}, Env) ->
  erruby_debug:debug_2("eval begin~n",[]),
  [ eval_ast(Ast,Env) || Ast <- Children ];

eval_ast({ast, type, self, children, []}, Env) ->
  #{ self := Self } = Env,
  Self;

eval_ast({ast, type, def, children, Children}, Env) ->
  [Name | [ {ast, type, args, children, Args} , Body ] ] = Children,
  #{ self := Self } = Env,
  erruby_object:def_method(Self, Name, Args, Body),
  Self;

eval_ast({ast,type,send, children, Children}, Env) ->
  erruby_debug:debug_1("send~n",[]),
  [print_ast(Ast) || Ast <- Children],
  [Receiver | [Msg | Args]] = Children,
  EvaledArgs = [ eval_ast(Child, Env) || Child <- Args],
  #{ self := Self } = Env,
  Target = case Receiver of
    undefined -> Self;
    _ -> eval_ast(Receiver)
  end,
  Method = erruby_object:find_method(Target, Msg),
  _Result = Method(EvaledArgs);

eval_ast({ast, type, lvasgn, children, Children}, #{ self := Self } = Env) ->
  [Name, ValAst] = Children,
  Val = eval_ast(ValAst, Env),
  erruby_object:lvasgn(Self, Name, Val);

eval_ast({ast, type, lvar, children, [Name]}, #{ self := Self} = _Env) ->
  {ok, Val} = erruby_object:lvar(Self, Name),
  Val;


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
  #{self => Kernal, lvars => #{}}.
