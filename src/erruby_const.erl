-module(erruby_const).
-behavior(gen_server).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).
-export([start_link/0]).

init([]) ->
  State = #{},
  {ok, State}.


start_link() ->
  gen_server:start_link(?MODULE, [], []).

terminate(_Arg, _State) ->
  {ok, dead}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_info(Info, State) ->
  io:format("Got unkwon info:~n~p~n", [Info]),
  {ok, State}.

handle_cast(_Req, State) ->
  io:format("handle unknown cast ~p ~p ~n",[_Req, State]),
  NewState = State,
  {reply, done, NewState}.

handle_call(_Req, _From, State) ->
  io:format("handle unknow call ~p ~p ~p ~n",[_Req, _From, State]),
  NewState = State,
  {reply, done, NewState}.
