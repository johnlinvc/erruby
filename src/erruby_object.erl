-module(erruby_object).
-behavior(gen_server).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).
%for vm
-export([def_method/4, find_method/2, def_const/3, find_const/2, init_object_class/0,object_class/0]).
%for other buildtin class
-export([def_method/3, new_object_with_pid_symbol/2]).
-export([init_class_class/0, class_class/0,new_class/0]).
-export([init_main_object/0, main_object/0]).

init([#{class := Class}]) ->
  DefaultState = default_state(),
  {ok, add_class_to_state(DefaultState, Class)};

init([]) ->
  {ok, default_state()}.

add_class_to_state(State, Class) ->
  State#{class => Class}.

%TODO in method_class return defalut class_class if no class is present
default_state() ->
  Methods = #{
    puts => fun method_puts/2,
    self => fun method_self/1,
    new => fun method_new/1,
    inspect => fun method_inspect/1,
    to_s => fun method_to_s/1
   },
  IVars = #{},
  Consts = #{'Object' => self()},
  #{self => self(), methods => Methods, ivars => IVars, consts => Consts}.


start_link() ->
  gen_server:start_link(?MODULE, [], []).

start_link(Class) ->
  gen_server:start_link(?MODULE, [#{class => Class }], []).

terminate(_Arg, _State) ->
  {ok, dead}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

get_class(Self) ->
  gen_server:call(Self, #{type => get_class}).

find_method(Self, Name) ->
  Klass = get_class(Self),
  gen_server:call(Klass, #{type => find_method, name => Name}).

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
  erruby_debug:debug_tmp("def_with_func:~p",[Func]),
  gen_server:call(Receiver, #{type => def_method, name => Name, func => Func}).


def_const(Self, Name, Value) ->
  gen_server:call(Self, #{type => def_const, name => Name, value => Value}).

find_const(Self, Name) ->
  gen_server:call(Self, #{type => find_const, name => Name}).



handle_info(Info, State) ->
  io:format("Got unkwon info:~n~p~n", [Info]),
  {ok, State}.

handle_call(#{ type := def_method , name := Name, body := Body, args := Args}=_Msg, _From, #{methods := Methods} =State) ->
  NewMethods = Methods#{ Name => #{ args => Args, body => Body, argc => length(Args) } },
  NewState = State#{ methods := NewMethods},
  {reply, Name, NewState};

handle_call(#{ type := def_method, name := Name, func := Func}=_Msg, _From, #{methods := Methods} = State) ->
  erruby_debug:debug_tmp("def_with_func",[]),
  NewMethods = Methods#{ Name => Func },
  NewState = State#{ methods := NewMethods},
  {reply, Name, NewState};

handle_call(#{ type := find_method, name := Name }, _From, State) ->
  #{methods := #{Name := Method}} = State,
  {reply, Method, State};

handle_call(#{ type := def_const, name := Name, value := Value }, _From, #{consts := Consts}=State) ->
  NewConsts = Consts#{Name => Value},
  NewState = State#{consts := NewConsts},
  {reply, Name, NewState};

handle_call(#{ type := find_const, name := Name }, _From, #{consts := Consts}=State) ->
  Value = maps:get(Name, Consts, nil),
  {reply, Value, State};

handle_call(#{ type := get_class}, _From, State) ->
  Value = maps:get(class, State, class_class()),
  {reply, Value, State};


handle_call(_Req, _From, State) ->
  io:format("handle unknow call ~p ~p ~p ~n",[_Req, _From, State]),
  NewState = State,
  {reply, done, NewState}.

handle_cast(_Req, State) ->
  io:format("handle unknown cast ~p ~p ~n",[_Req, State]),
  NewState = State,
  {reply, done, NewState}.

%TODO support va args
method_puts(Env, String) ->
  io:format("~s~n", [String]),
  Env#{ret_val => nil}.

method_self(#{self := Self}=Env) ->
  Env#{ret_val => Self}.

%FIXME new a real class
method_new(#{self := Klass}=Env) ->
  {ok, NewObject} = start_link(Klass),
  Env#{ret_val => NewObject}.

method_inspect(#{self := Self}=Env) ->
  S = io_lib:format("#<Object:~p>",[Self]),
  erruby_vm:new_string(S,Env).

method_to_s(#{self := Self}=Env) ->
  S = io_lib:format("~p",[Self]),
  erruby_vm:new_string(S,Env).

new_class() ->
  start_link(class_class()).

new_object_with_pid_symbol(Symbol, Class) ->
  gen_server:start_link({local, Symbol}, ?MODULE, [#{class => Class}], []).


%TODO use new_object_with_pid_symbol
%TODO lazy init
init_class_class() ->
  gen_server:start_link({local, erruby_class_class}, ?MODULE, [],[]).

init_object_class() ->
  gen_server:start_link({local, erruby_object_class}, ?MODULE, [],[]).

init_main_object() ->
  new_object_with_pid_symbol(erruby_main_object, object_class()).

object_class() ->
  whereis(erruby_object_class).

class_class() ->
  whereis(erruby_class_class).

main_object() ->
  whereis(erruby_main_object).

