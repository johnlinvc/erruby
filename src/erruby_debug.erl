-module(erruby_debug).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).
-export([start_link/1, debug/3, debug_1/2,debug_2/2, debug_tmp/2, set_debug_level/1]).
-export([print_env/1]).

init([DebugLevel]) ->
  {ok, #{debug_level => DebugLevel}}.

terminate(_Arg, _State) -> {ok, dead}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

debug(Format, Args, Level) ->
  FullFormat = lists:concat(["debug level ", Level, ": ", Format]),
  gen_server:call(erruby_debug_pid, #{level => Level, format => FullFormat, args => Args}).

debug_1(Format, Args) ->
  debug(Format, Args, 1).

debug_2(Format, Args) ->
  debug(Format, Args, 2).

debug_tmp(Format, Args) ->
  io:format(Format, Args).

set_debug_level(Level) ->
  gen_server:cast(erruby_debug_pid, #{new_level => Level}).

start_link(DebugLevel) -> gen_server:start_link({local, erruby_debug_pid} ,?MODULE, [DebugLevel], []).

handle_info(Info, State) ->
  io:format("Got unkwon info:~n~p~n", [Info]),
  {ok, State}.

handle_call(#{level := Level, format := Format, args := Args}, _From, #{debug_level := DebugLevel} = State) when DebugLevel >= Level ->
  io:format(Format, Args),
  {reply, ok, State};

handle_call(#{level := Level}, _From, #{debug_level := DebugLevel} = State) when DebugLevel < Level ->
  {reply, ok, State};

handle_call(_Req, _From, State) ->
  io:format("handle unknow call ~p ~p ~p ~n",[_Req, _From, State]),
  NewState = State,
  {reply, done, NewState}.

handle_cast(#{new_level := NewLevel}, State) ->
  {noreply, State#{ debug_level => NewLevel } };

handle_cast(_Req, State) ->
  io:format("handle unknown cast ~p ~p ~n",[_Req, State]),
  NewState = State,
  {noreply, NewState}.

print_env(Env) ->
  debug_1("Env: ~p ~n",[Env]).
