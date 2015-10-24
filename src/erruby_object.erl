-module(erruby_object).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, new_kernel/0, send/3]).

init([]) ->
  Methods = #{puts => fun method_puts/1},
  State = #{self => self(), methods => Methods},
  {ok, State}.

start_link() ->
  gen_server:start_link(?MODULE, [], []).

send(Self, Msg, Args) ->
  gen_server:call(Self, #{type => send, msg => Msg, args => Args}).

new_kernel() ->
  start_link().

handle_call(#{ type := send, msg := Msg, args:= Args}=_Req, _From, _State) ->
  io:format("obj send: ~p ~p ~p ~n",[_Req, _From, _State]),
  #{methods := #{Msg := Method}} = _State,
  io:format("method: ~p~n",[Method]),
  method_puts(Args),
  NewState = _State,
  {reply, done, NewState};

handle_call(_Req, _From, _State) ->
  io:format("~p ~p ~p ~n",[_Req, _From, _State]),
  NewState = _State,
  {reply, done, NewState}.

handle_cast(_Req, _State) ->
  io:format("~p ~p ~n",[_Req, _State]),
  NewState = _State,
  {reply, done, NewState}.

method_puts([Str]) ->
  io:format("~p~n", [Str]).
