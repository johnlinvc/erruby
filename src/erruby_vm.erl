-module(erruby_vm).
-export([eval_ast/1, scanl/3]).
-export([new_nil/1, new_string/2]).
-export([eval_method_with_exit/5]).

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

eval_ast({ast, type, nil, children, []}, Env) ->
  erruby_nil:new_nil(Env);

eval_ast({ast, type, true, children, []}, Env) ->
  erruby_boolean:new_true(Env);

eval_ast({ast, type, false, children, []}, Env) ->
  erruby_boolean:new_false(Env);

eval_ast({ast, type, array, children, Args}, Env) ->
  [_ |Envs] = scanl(fun eval_ast/2, Env, Args),
  EvaledArgs = lists:map( fun (T) -> #{ ret_val := R } = T, R end, Envs),
  LastEnv = lists:last(Envs),
  erruby_array:new_array(LastEnv, EvaledArgs);

eval_ast({ast, type, int, children, [N]}, Env) ->
  erruby_fixnum:new_fixnum(Env, N);

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
  Method = erruby_object:find_instance_method(Target, Msg),
  process_eval_method(Target,Method, EvaledArgs, LastEnv);

eval_ast({ast, type, block, children, [Method | [Args | [Body]]]= _Children}, Env) ->
  Block = #{body => Body, args => Args},
  BlockEnv = Env#{block => Block},
  Result = eval_ast(Method, BlockEnv),
  Result#{block => not_exist};

eval_ast({ast, type, yield, children, Args}, Env) ->
  [_ |Envs] = scanl(fun eval_ast/2, Env, Args),
  EvaledArgs = lists:map( fun (T) -> #{ ret_val := R } = T, R end, Envs),
  LastEnv = case Envs of
              [] -> Env;
              _ -> lists:last(Envs)
            end,
  #{block := #{body := Body, args := {ast, type, args, children, ArgNamesAst}}} = LastEnv,
  ArgNames = [ArgName || {ast, type, arg, children, [ArgName]} <- ArgNamesAst],
  NameWithArgs = lists:zip( ArgNames, EvaledArgs),
  NewFrameWithArgs = lists:foldl(fun ({Name, Arg}, EnvAcc) ->  bind_lvar(Name, Arg, EnvAcc) end, LastEnv, NameWithArgs),
  Result = eval_ast(Body,NewFrameWithArgs),
  Result;

eval_ast({ast, type, lvasgn, children, Children}, Env) ->
  [Name, ValAst] = Children,
  NewEnv = eval_ast(ValAst, Env),
  #{ret_val := RetVal } = NewEnv,
  bind_lvar(Name, RetVal, NewEnv);

%TODO also search for methods
eval_ast({ast, type, lvar, children, [Name]}, Env) ->
  erruby_debug:debug_1("searching lvar ~p~n in frame~p~n", [Name, Env]),
  #{ lvars := #{Name := Val}} = Env,
  Env#{ ret_val => Val};

eval_ast({ast, type, def, children, Children}, Env) ->
  [Name | [ {ast, type, args, children, Args} , Body ] ] = Children,
  #{ self := Self } = Env,
  erruby_object:def_method(Self, Name, Args, Body),
  new_symbol(Name, Env);

%TODO figure out the Unknown field in AST
%TODO impl ancestors
eval_ast({ast, type, class, children,
          [NameAst,undefined,Body] = _Children}, #{ self := Self } = Env) ->
  {_,_,const,_,[_,Name]} = NameAst,
  NameEnv = eval_ast(NameAst,Env),
  #{ret_val := ClassConst} = NameEnv,
  Class = case ClassConst of
    not_found -> {ok, NewClass} = erruby_class:new_class(),
           erruby_object:def_const(Self, Name, NewClass),
           NewClass;
      _ -> ClassConst
    end,
  NewFrame = new_frame(NameEnv, Class),
  ResultFrame = eval_ast(Body,NewFrame),
  pop_frame(ResultFrame);

%TODO refactor with the one without SupperClass
eval_ast({ast, type, class, children,
          [NameAst,SuperClassAst,Body] = _Children}, #{ self := Self } = Env) ->
  {_,_,const,_,[_,Name]} = NameAst,
  NameEnv = eval_ast(NameAst,Env),
  #{ret_val := ClassConst} = NameEnv,
  SuperClassEnv = eval_ast(SuperClassAst,NameEnv),
  #{ret_val := SuperClassConst} = SuperClassEnv,
  %TODO should fail when SuperClassConst is not defined
  Class = case ClassConst of
            not_found -> {ok, NewClass} = erruby_class:new_class(SuperClassConst),
                   erruby_object:def_const(Self, Name, NewClass),
                   NewClass;
            _ -> ClassConst
          end,
  case Body of
    undefined -> SuperClassEnv;
    _ ->
      NewFrame = new_frame(SuperClassEnv, Class),
      ResultFrame = eval_ast(Body,NewFrame),
      pop_frame(ResultFrame)
  end;



%TODO figure out the Unknown field in AST
%TODO add multi layer CONST def
eval_ast({ast, type, casgn, children, [Unknown, Name, ValAst] = Children}, #{ self := Self } = Env) ->
  NewEnv = eval_ast(ValAst, Env),
  #{ret_val := Val} = NewEnv,
  erruby_object:def_const(Self, Name, Val),
  NewEnv;

%TODO figure out the Unknown field in AST
%TODO add multi layer CONST find
%TODO throw error when not_found
eval_ast({ast, type, const, children, [Unknown, Name] = Children}, #{ self := Self } = Env) ->
  NestedConst = erruby_object:find_const(Self, Name),
  Const = case NestedConst of
            not_found -> erruby_object:find_const(erruby_object:object_class(), Name);
            _ -> NestedConst
          end,
  Env#{ret_val => Const};


eval_ast(Ast, Env) ->
  erruby_debug:debug_1("Unhandled eval~n",[]),
  print_ast(Ast),
  print_env(Env).


process_eval_method(Target,Method,Args,Env) ->
  Pid = spawn(?MODULE, eval_method_with_exit, [Target,Method,Args,Env, self()]),
  receive
    {method_result, Pid, Result} ->
      Result
  end.

eval_method_with_exit(Target,Method,Args,Env, Sender) ->
  try
    Result = eval_method(Target, Method, Args, Env),
    Respond = {method_result, self(),Result},
    Sender ! Respond
  catch
    _:E ->
      io:format("error ~p ~n", [E]),
      erlang:display(erlang:get_stacktrace())
  end.

eval_method(Target,Method, Args, Env) when is_function(Method) ->
  NewFrame = new_frame(Env,Target),
  MethodArgs = [NewFrame | Args],
  ResultFrame = apply(Method, MethodArgs),
  pop_frame(ResultFrame);

eval_method(Target,#{body := Body, args := ArgNamesAst} = _Method, Args, Env) ->
  NewFrame = new_frame(Env,Target),
  ArgNames = [ArgName || {ast, type, arg, children, [ArgName]} <- ArgNamesAst],
  NameWithArgs = lists:zip( ArgNames, Args),
  NewFrameWithArgs = lists:foldl(fun ({Name, Arg}, EnvAcc) ->  bind_lvar(Name, Arg, EnvAcc) end, NewFrame, NameWithArgs),
  pop_frame( eval_ast(Body, NewFrameWithArgs)).


bind_lvar(Name, Val, #{ lvars := LVars } = Env) ->
  Env#{ lvars := LVars#{ Name => Val }}.

receiver_or_self(undefined, Env) ->
  #{ self := Self } = Env,
  Env#{ret_val => Self};
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
  Env#{lvars => #{}, ret_val => not_exist, self => Self, prev_frame => Env}.

new_nil(Env) ->
  erruby_nil:new_nil(Env).

pop_frame(Frame) ->
  #{ret_val := RetVal, prev_frame := PrevFrame} = Frame,
  PrevFrame#{ret_val := RetVal}.

default_env() ->
  {ok, _ObjectClass} = erruby_object:init_object_class(),
  {ok, _ClassClass} = erruby_class:init_class_class(),
  {ok, MainObject} = erruby_object:init_main_object(),
  init_builtin_class(),
  #{self => MainObject, lvars => #{}}.

init_builtin_class() ->
  ok = erruby_nil:install_nil_class(),
  ok = erruby_array:install_array_classes(),
  ok = erruby_integer:install_integer_class(),
  ok = erruby_fixnum:install_fixnum_class(),
  ok = erruby_boolean:install_boolean_classes().
