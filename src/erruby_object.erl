-module(erruby_object).
-behavior(gen_server).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).
-export([new_kernel/0,  def_method/4, find_method/2, def_const/3, find_const/2, init_object_class/0,object_class/0]).

init([#{super_class := SuperClass}]) ->
  default_init();

init([]) ->
  default_init().

default_init() ->
  Methods = #{
    puts => fun method_puts/2,
    self => fun method_self/1,
    new => fun method_new/1
   },
  IVars = #{},
  Consts = #{'Object' => self()},
  State = #{self => self(), methods => Methods, ivars => IVars, consts => Consts},
  {ok, State}.


start_link() ->
  gen_server:start_link(?MODULE, [], []).

start_link(SuperClass) ->
  gen_server:start_link(?MODULE, [SuperClass], []).


terminate(_Arg, _State) ->
  {ok, dead}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.


find_method(Self, Name) ->
  gen_server:call(Self, #{type => find_method, name => Name}).

%TODO: we need frame when running
def_method(Self, Name, Args, Body) ->
  gen_server:call(Self, #{type => def_method, name => Name, args => Args, body => Body}).

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

method_new(#{self := Self}=Env) ->
  Env#{ret_val => Self}.

new_kernel() ->
  start_link().

init_object_class() ->
  gen_server:start_link({local, erruby_object_class}, ?MODULE, [],[]).

object_class() ->
  whereis(erruby_object_class).

