-module(erruby_object).
-behavior(gen_server).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).
-export([new_kernel/0, lvasgn/3, lvar/2, def_method/4, find_method/2]).

init([]) ->
  Methods = #{puts => fun method_puts/1},
  IVars = #{},
  State = #{self => self(), methods => Methods, ivars => IVars},
  {ok, State}.

start_link() ->
  gen_server:start_link(?MODULE, [], []).

terminate(_Arg, _State) ->
  {ok, dead}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

lvasgn(Self, Name, Val) ->
  gen_server:call(Self, #{type => lvasgn, name => Name, val => Val}).

lvar(Self, Name) ->
  gen_server:call(Self, #{type => lvar, name => Name}).

find_method(Self, Name) ->
  gen_server:call(Self, #{type => find_method, name => Name}).

%TODO: we need frame when running
def_method(Self, Name, Args, Body) ->
  gen_server:call(Self, #{type => def_method, name => Name, args => Args, body => Body}).

new_kernel() ->
  start_link().

handle_info(Info, State) ->
  io:format("Got unkwon info:~n~p~n", [Info]),
  {ok, State}.

handle_call(#{ type := def_method }=Msg, _From, State) ->
  io:format("Got def_method:~n~p~n", [Msg]),
  {reply, done, State};

handle_call(#{ type := find_method, name := Name }, _From, State) ->
  #{methods := #{Name := Method}} = State,
  {reply, Method, State};

handle_call(#{ type := lvasgn, name := Name, val:= Val }=_Req, _From, #{ivars := IVars}=State) ->
  NewState = State#{ivars := IVars#{Name => Val}},
  %TODO customer operation overloading
  {reply, Val, NewState};

handle_call(#{ type := lvar, name := Name}, _From, #{ivars := IVars}=State) ->
  case maps:is_key(Name, IVars) of
    true ->
      #{Name := Val} = IVars,
      {reply, {ok, Val}, State};
    false ->
      %TODO find with method
      {reply, {error, 'undefined local variable'}, State}
  end;

handle_call(_Req, _From, State) ->
  io:format("handle unknow call ~p ~p ~p ~n",[_Req, _From, State]),
  NewState = State,
  {reply, done, NewState}.

handle_cast(_Req, State) ->
  io:format("handle unknown cast ~p ~p ~n",[_Req, State]),
  NewState = State,
  {reply, done, NewState}.

method_puts(Strings) ->
  [ io:format("~s~n", [Str]) || Str <- Strings ].
