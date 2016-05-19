-module(erruby_vm).
-include("rb.hrl").
-export([eval_file/2, scanl/3]).
-export([new_nil/1, new_string/2]).
-export([eval_method_with_exit/5, yield/2]).
-export([file_name/1]).

print_ast(Ast) ->
  erruby_debug:debug_1("Ast: ~p ~n",[Ast]).

print_env(Env) ->
  erruby_debug:debug_1("Env: ~p ~n",[Env]).

scanl(_F, Acc, []) ->
  [Acc];
scanl(F, Acc0, [H | T]) ->
  Acc = apply(F, [H, Acc0]),
  [Acc0 | scanl(F, Acc, T)].

eval_file(Ast, FileName) ->
  DefaultEnv = default_env(),
  Env = set_filename(DefaultEnv, FileName),
  eval_ast(Ast, Env).

set_filename(Env, FileName) ->
  Env#{'FileName' => FileName}.

file_name(Env) ->
  case maps:find('FileName', Env) of
    {ok, Value} -> Value;
    error ->
      file_name(find_prev_frame(Env))
  end.

eval_ast({ast,type,'begin',children, Children}, Env) ->
  erruby_debug:debug_2("eval begin~n",[]),
  lists:foldl(fun eval_ast/2, Env, Children);

eval_ast({ast, type, self, children, []}, Env) ->
  #{ self := Self } = Env,
  erruby_rb:return(Self, Env);

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
  {EvaledArgs, LastEnv} = eval_args(Args, Env),
  erruby_array:new_array(LastEnv, EvaledArgs);

eval_ast({ast, type, int, children, [N]}, Env) ->
  erruby_fixnum:new_fixnum(Env, N);

%TODO call method using method object
eval_ast({ast,type,send, children, Children}, Env) ->
  erruby_debug:debug_1("send~n",[]),
  [print_ast(Ast) || Ast <- Children],
  [Receiver | [Msg | Args]] = Children,
  ReceiverFrame = receiver_or_self(Receiver, Env),
  Target = erruby_rb:ret_val(ReceiverFrame),
  {EvaledArgs, LastEnv} = eval_args(Args, ReceiverFrame),
  Method = erruby_object:find_instance_method(Target, Msg),
  process_eval_method(Target,Method, EvaledArgs, LastEnv);

eval_ast({ast, type, block, children, [Method | [Args | [Body]]]= _Children}, Env) ->
  Block = #{body => Body, args => Args},
  BlockEnv = Env#{block => Block},
  Result = eval_ast(Method, BlockEnv),
  Result#{block => not_exist};

eval_ast({ast, type, yield, children, Args}, Env) ->
  {EvaledArgs, LastEnv} = eval_args(Args, Env),
  yield(LastEnv, EvaledArgs);

%FIXME return the value
eval_ast({ast, type, lvasgn, children, Children}, Env) ->
  [Name, ValAst] = Children,
  NewEnv = eval_ast(ValAst, Env),
  RetVal = erruby_rb:ret_val(NewEnv),
  bind_lvar(Name, RetVal, NewEnv);

eval_ast({ast, type, lvar, children, [Name]}, Env) ->
  erruby_debug:debug_1("searching lvar ~p~n in frame~p~n", [Name, Env]),
  #{ lvars := #{Name := Val}} = Env,
  erruby_rb:return(Val, Env);

%FIXME return the value
eval_ast({ast, type, ivasgn, children, Children}, #{self := Self}=Env) ->
  [Name, ValAst] = Children,
  NewEnv = eval_ast(ValAst, Env),
  RetVal = erruby_rb:ret_val(NewEnv),
  erruby_object:def_ivar(Self, Name, RetVal),
  erruby_rb:return(RetVal, Env);

eval_ast({ast, type, ivar, children, [Name]}, #{self := Self}=Env) ->
  Val = erruby_object:find_ivar(Self, Name),
  erruby_rb:return(Val, Env);

eval_ast({ast, type, gvasgn, children, Children}, Env) ->
  [Name, ValAst] = Children,
  NewEnv = eval_ast(ValAst, Env),
  RetVal = erruby_rb:ret_val(NewEnv),
  erruby_object:def_global_var(Name, RetVal),
  erruby_rb:return(Name, NewEnv);

eval_ast({ast, type, gvar, children, [Name]}, Env) ->
  Val = erruby_object:find_global_var(Name),
  erruby_rb:return(Val, Env);

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
  ClassConst = erruby_rb:ret_val(NameEnv),
  Class = case ClassConst of
    not_found -> {ok, NewClass} = erruby_class:new_class(),
           erruby_object:def_const(Self, Name, NewClass),
           NewClass;
      _ -> ClassConst
    end,
  case Body of
    undefined -> NameEnv;
    _ ->
      NewFrame = new_frame(NameEnv, Class),
      ResultFrame = eval_ast(Body,NewFrame),
      pop_frame(ResultFrame)
  end;

%TODO refactor with the one without SupperClass
eval_ast({ast, type, class, children,
          [NameAst,SuperClassAst,Body] = _Children}, #{ self := Self } = Env) ->
  {_,_,const,_,[_,Name]} = NameAst,
  NameEnv = eval_ast(NameAst,Env),
  ClassConst = erruby_rb:ret_val(NameEnv),
  SuperClassEnv = eval_ast(SuperClassAst,NameEnv),
  SuperClassConst = erruby_rb:ret_val(SuperClassEnv),
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



eval_ast({ast, type, casgn, children, [ParentConstAst, Name, ValAst] }, Env) ->
  ParentConstEnv = parent_const_env(ParentConstAst, Env),
  ParentConst = erruby_rb:ret_val(ParentConstEnv),
  NewEnv = eval_ast(ValAst, ParentConstEnv),
  Val = erruby_rb:ret_val(NewEnv),
  erruby_object:def_const(ParentConst, Name, Val),
  NewEnv;

%TODO throw error when not_found
eval_ast({ast, type, const, children, [ParentConstAst, Name]}, Env) ->
  ParentConstEnv = parent_const_env(ParentConstAst, Env),
  ParentConst = erruby_rb:ret_val(ParentConstEnv),
  LocalConst = erruby_object:find_const(ParentConst, Name),
  Const = case LocalConst of
            not_found -> erruby_object:find_const(erruby_object:object_class(), Name);
            _ -> LocalConst
          end,
  erruby_rb:return(Const, Env);


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
  end,
  exit(normal).

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
  erruby_rb:return(Self, Env);
receiver_or_self(Receiver, Env) ->
  eval_ast(Receiver,Env).


new_string(String, Env) ->
  erruby_rb:return(String, Env).

new_symbol(Symbol, Env) ->
  erruby_rb:return(Symbol, Env).

new_frame(Env, Self) ->
  Env#{lvars => #{}, ret_val => not_exist, self => Self, prev_frame => Env}.

new_nil(Env) ->
  erruby_nil:new_nil(Env).

%TODO move to another place
find_prev_frame(Env) ->
  case maps:get(prev_frame, Env, no_prev_frame) of
    no_prev_frame -> throw(cant_find_block);
    Frame -> Frame
  end.

find_block(Env) ->
  case maps:get(block, Env) of
    not_exist -> find_block(find_prev_frame(Env));
    Block -> Block
  end.

parent_const_env(ParentConstAst, Env) ->
  case ParentConstAst of
    undefined -> erruby_rb:ret_self(Env);
    {ast, type, const, children, _} -> eval_ast(ParentConstAst, Env)
  end.

eval_args(ArgAsts, Env) ->
  [_ |Envs] = scanl(fun eval_ast/2, Env, ArgAsts),
  EvaledArgs = lists:map( fun erruby_rb:ret_val/1, Envs),
  LastEnv = case Envs of
              [] -> Env;
              _ -> lists:last(Envs)
            end,
  {EvaledArgs, LastEnv}.


yield(Env, Args)->
  Block = find_block(Env),
  #{body := Body, args := {ast, type, args, children, ArgNamesAst}} = Block,
  ArgNames = [ArgName || {ast, type, arg, children, [ArgName]} <- ArgNamesAst],
  NameWithArgs = lists:zip( ArgNames, Args),
  NewFrameWithArgs = lists:foldl(fun ({Name, Arg}, EnvAcc) ->  bind_lvar(Name, Arg, EnvAcc) end, Env, NameWithArgs),
  Result = eval_ast(Body,NewFrameWithArgs),
  Result.

pop_frame(Frame) ->
  #{ret_val := RetVal, prev_frame := PrevFrame} = Frame,
  erruby_rb:return(RetVal, PrevFrame).

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
