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

eval_ast({ast,type,'begin',children, Children}, Env) ->
  erruby_debug:debug_2("eval begin~n",[]),
  lists:foldl(fun eval_ast/2, Env, Children);

eval_ast({ast, type, self, children, []}, Env) ->
  #{ self := Self } = Env,
  Env#{ret_val => Self};

eval_ast({ast, type, str, children, Children}, Env) ->
  [SBin|_T] = Children,
  new_string(binary_to_list(SBin), Env);

%TODO call method using method object
eval_ast({ast,type,send, children, Children}, Env) ->
  erruby_debug:debug_1("send~n",[]),
  [print_ast(Ast) || Ast <- Children],
  [Receiver | [Msg | Args]] = Children,
  ReceiverFrame = receiver_or_self(Receiver, Env),
  #{ ret_val := Target} = ReceiverFrame,
  [_ |Envs] = scanl(fun eval_ast/2, ReceiverFrame, Args),
  EvaledArgs = lists:map( fun (T) -> #{ ret_val := R } = T, R end, Envs),
  LastEnv = case Envs of
              [] -> ReceiverFrame;
              _ -> lists:last(Envs)
            end,
  Method = erruby_object:find_method(Target, Msg),
  eval_method(Target,Method, EvaledArgs, LastEnv);

eval_ast({ast, type, block, children, [Method | [Args | [Body]]]= Children}, Env) ->
  %erruby_debug:debug_tmp("block~n",[]),
  Block = #{body => Body, args => Args},
  BlockEnv = Env#{block => Block},
  Result = eval_ast(Method, BlockEnv),
  Result#{block => nil};

eval_ast({ast, type, yield, children, Children}, Env) ->
  %erruby_debug:debug_tmp("yield block~n",[]),
  [print_ast(Ast) || Ast <- Children],
  #{block := #{body := Body, args := Args}} = Env,
  eval_ast(Body,Env);

eval_ast({ast, type, lvasgn, children, Children}, Env) ->
  [Name, ValAst] = Children,
  NewEnv = eval_ast(ValAst, Env),
  #{ret_val := RetVal } = NewEnv,
  bind_lvar(Name, RetVal, NewEnv);

%TODO also search for local vars
eval_ast({ast, type, lvar, children, [Name]}, Env) ->
  #{ lvars := #{Name := Val}} = Env,
  Env#{ ret_val => Val};

eval_ast({ast, type, def, children, Children}, Env) ->
  [Name | [ {ast, type, args, children, Args} , Body ] ] = Children,
  #{ self := Self } = Env,
  erruby_object:def_method(Self, Name, Args, Body),
  new_symbol(Name, Env);

eval_ast(Ast, Env) ->
  erruby_debug:debug_1("Unhandled eval~n",[]),
  print_ast(Ast),
  print_env(Env).

eval_method(Target,Method, Args, Env) when is_function(Method) ->
  Method( new_frame(Env,Target) ,Args );

eval_method(Target,#{body := Body, args := ArgNamesAst} = _Method, Args, Env) ->
  NewFrame = new_frame(Env,Target),
  ArgNames = [ArgName || {ast, type, arg, children, [ArgName]} <- ArgNamesAst],
  NameWithArgs = lists:zip( ArgNames, Args),
  NewFrameWithArgs = lists:foldl(fun ({Name, Arg}, EnvAcc) ->  bind_lvar(Name, Arg, EnvAcc) end, NewFrame, NameWithArgs),
  #{ret_val := RetVal} = eval_ast(Body, NewFrameWithArgs),
  Env#{ret_val := RetVal}.


bind_lvar(Name, Val, #{ lvars := LVars } = Env) ->
  Env#{ lvars := LVars#{ Name => Val }}.

receiver_or_self(undefined, Env) ->
  #{ self := Self } = Env,
  Env#{ret_val := Self};
receiver_or_self(Receiver, Env) ->
  eval_ast(Receiver,Env).

eval_ast(Ast) ->
  Env = default_env(),
  eval_ast(Ast, Env).

new_string(String, Env) ->
  Env#{ret_val => String}.

new_symbol(Symbol, Env) ->
  Env#{ret_val => Symbol}.

new_frame(Env, Self) ->
  Env#{lvars => #{}, ret_val => nil, self => Self, prev_frame => Env}.

default_env() ->
  {ok, Kernal} = erruby_object:new_kernel(),
  #{self => Kernal, lvars => #{}}.
