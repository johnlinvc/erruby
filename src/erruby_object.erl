-module(erruby_object).
-behavior(gen_server).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2, new_kernel/0, msg_send/3]).

init([]) ->
  Methods = #{puts => fun method_puts/1},
  State = #{self => self(), methods => Methods},
  {ok, State}.

start_link() ->
  gen_server:start_link(?MODULE, [], []).

terminate(_Arg, _State) ->
  {ok, dead}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

msg_send(Self, Msg, Args) ->
  gen_server:call(Self, #{type => msg_send, msg => Msg, args => Args}).

new_kernel() ->
  start_link().

handle_info(Info, State) ->
  io:format("Got unkwon info:~n~p~n", [Info]),
  {ok, State}.

handle_call(#{ type := msg_send, msg := Msg, args:= Args}=_Req, _From, State) ->
  #{methods := #{Msg := Method}} = State,
  Method(Args),
  NewState = State,
  {reply, done, NewState};

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
