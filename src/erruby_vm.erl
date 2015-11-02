-module(erruby_vm).
-export([eval_ast/1, scanl/3]).

print_ast(Ast) ->
  erruby_debug:debug_1("Ast: ~p ~n",[Ast]).

print_env(Env) ->
  erruby_debug:debug_1("Env: ~p ~n",[Env]).

scanl(_F, Acc, []) ->
  [Acc];
scanl(F, Acc0, [H | T]) ->
  Acc = apply(F, [H, Acc0]),
  [Acc0 | scanl(F, Acc, T)].

eval_ast_rev(Env,Ast) ->
  eval_ast(Ast,Env).

eval_ast({ast,type,'begin',children, Children}, Env) ->
  erruby_debug:debug_2("eval begin~n",[]),
  lists:foldl(fun eval_ast_rev/2, Env, Children);

eval_ast({ast, type, self, children, []}, Env) ->
  #{ self := Self } = Env,
  Env#{ret_val => Self};

eval_ast({ast, type, str, children, Children}, Env) ->
  [SBin|_T] = Children,
  Env#{ret_val => binary_to_list(SBin)};

eval_ast({ast,type,send, children, Children}, Env) ->
  erruby_debug:debug_1("send~n",[]),
  [print_ast(Ast) || Ast <- Children],
  [Receiver | [Msg | Args]] = Children,
  [_ |Envs] = scanl(fun eval_ast/2, Env, Args),
  EvaledArgs = lists:map( fun (T) -> #{ ret_val := R } = T, R end, Envs),
  #{ self := Self } = Env,
  Target = case Receiver of
    undefined -> Self;
    _ -> eval_ast(Receiver)
  end,
  Method = erruby_object:find_method(Target, Msg),
  Result = Method(EvaledArgs),
  LastEnv = lists:last(Envs),
  LastEnv#{ret_val => Result};

%old methods

eval_ast({ast, type, def, children, Children}, Env) ->
  [Name | [ {ast, type, args, children, Args} , Body ] ] = Children,
  #{ self := Self } = Env,
  erruby_object:def_method(Self, Name, Args, Body),
  Self;


eval_ast({ast, type, lvasgn, children, Children}, #{ self := Self } = Env) ->
  [Name, ValAst] = Children,
  Val = eval_ast(ValAst, Env),
  erruby_object:lvasgn(Self, Name, Val);

eval_ast({ast, type, lvar, children, [Name]}, #{ self := Self} = _Env) ->
  {ok, Val} = erruby_object:lvar(Self, Name),
  Val;

eval_ast(Ast, Env) ->
  erruby_debug:debug_1("Unhandled eval~n",[]),
  print_ast(Ast),
  print_env(Env).

eval_ast(Ast) ->
  Env = default_env(),
  eval_ast(Ast, Env).

default_env() ->
  {ok, Kernal} = erruby_object:new_kernel(),
  #{self => Kernal, lvars => #{}}.
