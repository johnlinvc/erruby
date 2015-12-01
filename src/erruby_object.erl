-module(erruby_object).
-behavior(gen_server).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).
-export([new_kernel/0,  def_method/4, find_method/2, def_const/3, find_const/2]).

init([]) ->
  Methods = #{
    puts => fun method_puts/2,
    self => fun method_self/1
   },
  IVars = #{},
  Consts = #{'Object' => self()},
  State = #{self => self(), methods => Methods, ivars => IVars, consts => Consts},
  {ok, State}.

start_link() ->
  gen_server:start_link(?MODULE, [], []).

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
  #{Name := Value} = Consts,
  {reply, Value, State};

handle_call(_Req, _From, State) ->
  io:format("handle unknow call ~p ~p ~p ~n",[_Req, _From, State]),
  NewState = State,
  {reply, done, NewState}.

handle_cast(_Req, State) ->
  io:format("handle unknown cast ~p ~p ~n",[_Req, State]),
  NewState = State,
  {reply, done, NewState}.

method_puts(Env, Strings) ->
  [ io:format("~s~n", [Str]) || Str <- Strings ],
  Env#{ret_val => nil}.

method_self(#{self := Self}=Env) ->
  Env#{ret_val => Self}.

new_kernel() ->
  start_link().
