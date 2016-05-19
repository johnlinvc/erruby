-module(erruby_object).
-include("rb.hrl").
-behavior(gen_server).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).
%for vm
-export([def_method/4, find_instance_method/2, def_global_const/2, find_global_const/1, def_const/3, find_const/2, init_object_class/0,object_class/0]).
-export([def_global_var/2, find_global_var/1]).
%for other buildtin class
-export([def_method/3, new_object_with_pid_symbol/2, new_object/2]).
-export([def_ivar/3, find_ivar/2]).
-export([init_main_object/0, main_object/0]).
-export([start_link/2, start_link/1]).
-export([get_properties/1, set_properties/2]).

init([#{class := Class, properties := Properties}]) ->
  DefaultState = default_state(),
  StateWithClass = add_class_to_state(DefaultState, Class),
  {ok, add_property_to_state(StateWithClass, Properties)};

init([#{class := Class}]) ->
  DefaultState = default_state(),
  {ok, add_class_to_state(DefaultState, Class)};

init([]) ->
  {ok, default_state()}.

add_class_to_state(State, Class) ->
  State#{class => Class}.

add_property_to_state(State, Properties) ->
  State#{properties => Properties}.

%TODO in method_class return defalut object_class if no class is present
default_state() ->
  Methods = #{},
  IVars = #{},
  Consts = #{},
  #{self => self(),
    methods => Methods,
    ivars => IVars,
    properties => #{},
    consts => Consts}.


%TODO unify these?
start_link(Class) ->
  gen_server:start_link(?MODULE, [#{class => Class }], []).

start_link(Class, Properties) ->
  gen_server:start_link(?MODULE, [#{class => Class, properties => Properties}], []).

terminate(_Arg, _State) ->
  {ok, dead}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

get_class(Self) ->
  gen_server:call(Self, #{type => get_class}).

find_instance_method(Self, Name) ->
  Klass = get_class(Self),
  gen_server:call(Klass, #{type => find_method, name => Name}).

find_method(Self, Name) ->
  gen_server:call(Self, #{type => find_method, name => Name}).

self_or_object_class(Self) ->
  MainObject = main_object(),
  case Self of
    MainObject -> object_class();
    _ -> Self
  end.


def_method(Self, Name, Args, Body) ->
  Receiver = self_or_object_class(Self),
  gen_server:call(Receiver, #{type => def_method, name => Name, args => Args, body => Body}).

def_method(Self,Name,Func) when is_function(Func) ->
  Receiver = self_or_object_class(Self),
  gen_server:call(Receiver, #{type => def_method, name => Name, func => Func}).

%TODO call def_const instead
def_global_const(Name, Value) ->
  gen_server:call(object_class(), #{type => def_const, name => Name, value => Value}).

find_global_const(Name) ->
  find_const(object_class(), Name).


%TODO define on basic object instead
%TODO ability to use custom getter/setter
def_global_var(Name, Value) ->
  Msg = #{type => def_global_var, name => Name, value => Value},
  gen_server:call(object_class(), Msg).

find_global_var(Name) ->
  gen_server:call(object_class(), #{type => find_global_var, name => Name}).

def_const(Self, Name, Value) ->
  Receiver = self_or_object_class(Self),
  gen_server:call(Receiver, #{type => def_const, name => Name, value => Value}).

find_const(Self, Name) ->
  erruby_debug:debug_2("finding on ~p for const:~p~n",[Self, Name]),
  gen_server:call(Self, #{type => find_const, name => Name}).

def_ivar(Self, Name, Value)->
  gen_server:call(Self, #{type => def_ivar, name => Name, value => Value}).

find_ivar(Self, Name) ->
  erruby_debug:debug_2("finding on ~p for ivar:~p~n",[Self, Name]),
  gen_server:call(Self, #{type => find_ivar, name => Name}).

get_properties(Self) ->
  gen_server:call(Self, #{type => get_properties}).

set_properties(Self, Properties) ->
  gen_server:call(Self, #{type => set_properties, properties => Properties}).


handle_info(Info, State) ->
  io:format("Got unkwon info:~n~p~n", [Info]),
  {ok, State}.

handle_call(#{ type := def_method , name := Name, body := Body, args := Args}=_Msg, _From, #{methods := Methods} =State) ->
  NewMethods = Methods#{ Name => #{ args => Args, body => Body, argc => length(Args) } },
  NewState = State#{ methods := NewMethods},
  {reply, Name, NewState};

handle_call(#{ type := def_method, name := Name, func := Func}=_Msg, _From, #{methods := Methods} = State) ->
  NewMethods = Methods#{ Name => Func },
  NewState = State#{ methods := NewMethods},
  {reply, Name, NewState};

handle_call(#{ type := find_method, name := Name }, _From, #{methods := Methods} = State) ->
  erruby_debug:debug_2("finding method:~p~n in State:~p~n",[Name, State]),
  case maps:is_key(Name,Methods) of
    true ->
      #{Name := Method} = Methods,
      {reply, Method, State};
    false ->
      %TODO use error classes
      %io:format("Method ~p not found~n",[Name]),
      erruby_debug:debug_2("finding in ancestors:~p~n",[ancestors(State)]),
      Method = find_method_in_ancestors(ancestors(State), Name),
      {reply, Method, State}
  end;

handle_call(#{ type := get_properties }, _From, #{properties := Properties}=State) ->
  {reply, Properties, State};

handle_call(#{ type := set_properties, properties := Properties }, _From, State) ->
  NewState = State#{ properties := Properties},
  {reply, NewState, NewState};

handle_call(#{ type := def_ivar, name := Name, value := Value }, _From, #{ivars := IVars}=State) ->
  NewIvars = IVars#{Name => Value},
  NewState = State#{ivars := NewIvars},
  {reply, Name, NewState};

handle_call(#{ type := find_ivar, name := Name }, _From, #{ivars := IVars}=State) ->
  erruby_debug:debug_2("finding ivar:~p~nin State:~p~n",[Name, State]),
  Value = maps:get(Name, IVars, erruby_nil:nil_instance()),
  {reply, Value, State};

handle_call(#{ type := def_const, name := Name, value := Value }, _From, #{consts := Consts}=State) ->
  NewConsts = Consts#{Name => Value},
  NewState = State#{consts := NewConsts},
  {reply, Name, NewState};

handle_call(#{ type := find_const, name := Name }, _From, #{consts := Consts}=State) ->
  erruby_debug:debug_2("finding const:~p~nin State:~p~n",[Name, State]),
  Value = maps:get(Name, Consts, not_found),
  {reply, Value, State};

handle_call(#{ type := def_global_var, name := Name, value := Value}, _From,
            #{properties := #{global_var_tbl := GVarTbl} } = State) ->
  NewGVarTbl = GVarTbl#{ Name => Value},
  #{properties := Properties} = State,
  NewProperties = Properties#{ global_var_tbl := NewGVarTbl },
  NewState = State#{ properties :=  NewProperties},
  {reply, Name, NewState};

handle_call(#{ type := find_global_var, name := Name}, _From,
            #{properties := #{global_var_tbl := GVarTbl} } = State) ->
  Value = maps:get(Name, GVarTbl, not_found),
  {reply, Value, State};


handle_call(#{ type := get_class}, _From, State) ->
  Value = maps:get(class, State, object_class()),
  {reply, Value, State};


handle_call(_Req, _From, State) ->
  io:format("handle unknow call ~p ~n ~p ~n ~p ~n",[_Req, _From, State]),
  NewState = State,
  {reply, done, NewState}.

handle_cast(_Req, State) ->
  io:format("handle unknown cast ~p ~p ~n",[_Req, State]),
  NewState = State,
  {reply, done, NewState}.

%TODO support va args
method_puts(Env, String) ->
  io:format("~s~n", [String]),
  erruby_nil:new_nil(Env).

append_rb_extension(FileName) ->
  case filename:extension(FileName) of
    [] -> string:concat(FileName, ".rb");
    _ -> FileName
  end.

%TODO extract to Kernal
%TODO raise error if file not found
method_require_relative(Env, FileName) ->
  RelativeFileName = relativeFileName(Env, FileName),
  RelativeFileNameWithExt = append_rb_extension(RelativeFileName),
  LoadedFeatures = find_global_var("$LOADED_FEATURES"),
  LoadedFeaturesList = erruby_array:array_to_list(LoadedFeatures),
  Contains = lists:member( RelativeFileNameWithExt, LoadedFeaturesList),
  case Contains of
    true -> erruby_boolean:new_false(Env);
    _ ->
      load_file(Env, RelativeFileNameWithExt),
      erruby_array:push(LoadedFeatures, RelativeFileNameWithExt),
      erruby_boolean:new_true(Env)
  end.

relativeFileName(Env, FileName) ->
  SrcFile = erruby_vm:file_name(Env),
  SrcDir = filename:dirname(SrcFile),
  filename:join([SrcDir, FileName]).

load_file(Env, RelativeFileNameWithExt) ->
  try
    erruby:eruby(RelativeFileNameWithExt),
    erruby_boolean:new_true(Env)
  catch
    _:_E ->
      erruby_debug:debug_2("cant require_relative file ~p~n", [RelativeFileNameWithExt]),
      erruby_boolean:new_false(Env)
  end.

%TODO raise error if file not found
% @TODO find a better way to get filename
method_load(Env, FileName)->
  Pwd = os:getenv("PWD"),
  RelativeFileNameWithExt = filename:join([Pwd, FileName]),
  load_file(Env, RelativeFileNameWithExt).

method_self(#{self := Self}=Env) ->
  erruby_rb:return(Self, Env).

method_inspect(#{self := Self}=Env) ->
  S = io_lib:format("#<Object:~p>",[Self]),
  erruby_vm:new_string(S,Env).

method_to_s(#{self := Self}=Env) ->
  S = io_lib:format("~p",[Self]),
  erruby_vm:new_string(S,Env).

%TODO support property?
new_object_with_pid_symbol(Symbol, Class) ->
  gen_server:start_link({local, Symbol}, ?MODULE, [#{class => Class}], []).

new_object(Class, Payload) when is_map(Payload) ->
  start_link(Class, Payload).

init_object_class() ->
  erb:find_or_init_class(erruby_object_class, fun init_object_class_internal/0).

init_object_class_internal() ->
  {ok, Pid} = gen_server:start_link({local, erruby_object_class}, ?MODULE, [],[]),
  install_object_class_methods(),
  'Object' = def_const(Pid, 'Object', Pid),
  set_properties(object_class(), #{global_var_tbl => #{}}),
  def_global_var("$LOADED_FEATURES", erruby_array:new_array([])),
  {ok, Pid}.

init_main_object() ->
  erb:find_or_init_class(erruby_main_object, fun init_main_object_internal/0).

init_main_object_internal() ->
  new_object_with_pid_symbol(erruby_main_object, object_class()).

object_class() ->
  whereis(erruby_object_class).

main_object() ->
  whereis(erruby_main_object).

install_object_class_methods() ->
  %TODO use this after inherent is done
  %def_method(object_class(), '==', fun method_eq/2).
  def_method(object_class(), 'puts', fun method_puts/2),
  def_method(object_class(), 'self', fun method_self/1),
  def_method(object_class(), 'inspect', fun method_inspect/1),
  def_method(object_class(), 'to_s', fun method_to_s/1),
  def_method(object_class(), '==', fun method_eq/2),
  def_method(object_class(), 'require_relative', fun method_require_relative/2),
  def_method(object_class(), 'load', fun method_load/2),
  ok.


method_eq(#{self := Self}=Env, Object) ->
  case Object of
    Self -> erruby_boolean:new_true(Env);
    _ -> erruby_boolean:new_false(Env)
  end.

super_class(#{properties := Properties}=_State) ->
  maps:get(superclass, Properties, object_class()).

%TODO handle include & extend
ancestors(State) ->
  SuperClass = super_class(State),
  ObjectClass = object_class(),
  case self() of
    ObjectClass -> [];
    _ -> [SuperClass, ObjectClass]
  end.

find_method_in_ancestors([], _Name) ->
  not_found;

find_method_in_ancestors(Ancestors, Name) ->
  [Klass | Rest] = Ancestors,
  Method = find_method(Klass, Name),
  case Method of
    not_found -> find_method_in_ancestors(Rest, Name);
    _ -> Method
  end.
