-module(erruby).
-export([eruby/1, start_ruby/0, stop_ruby/1, parse_ast/2, main/1]).

opt_spec_list() ->
  [
   {debug, $d, "debug", {integer, 0}, "Verbose level for debugging"},
   {help, $h, "help", undefined, "Show this help"}
  ].

handle_opts([{debug, DebugLevel} | T]) ->
  erruby_debug:set_debug_level(DebugLevel),
  handle_opts(T);
handle_opts([help | T]) ->
  show_help(),
  handle_opts(T);
handle_opts(_Opts) ->
  ok.

main(Args) ->
  add_lib_path(),
  erruby_debug:start_link(0),
  {ok, {Opts, Extra}} = getopt(Args),
  handle_opts(Opts),
  [SrcFileName | RubyArgs] = Extra,
  try
    erruby_debug:debug_1("input file name ~s\n", [SrcFileName]),
    erruby_debug:debug_1("input args ~s\n", [RubyArgs]),
    eruby(SrcFileName)
  catch
    _:E ->
      io:format("error ~p ~n", [E]),
      erlang:display(erlang:get_stacktrace())
  end.

eruby(SrcFileName) ->
  {ok, Binary} = file:read_file(SrcFileName),
  FileLines = binary:bin_to_list(Binary),
  Ruby = start_ruby(),
  Ast = parse_ast(Ruby, FileLines),
  stop_ruby(Ruby),
  erruby_vm:eval_ast(Ast).


getopt(Args) ->
  getopt:parse(opt_spec_list(), Args).

show_help() ->
  getopt:usage(opt_spec_list(), "erruby", "[programfile] [arguments]"),
  halt(1).


install_encoder(Ruby) ->
  ruby:call(Ruby, './rb_src/erruby.rb', 'install_encoder',[]).


parse_ast(Ruby, String) ->
  ruby:call(Ruby, './rb_src/erruby.rb','parse', [String]).

add_lib_path() ->
  code:add_path("./deps/erlport/ebin"),
  code:add_path("./deps/getopt/ebin"),
  code:add_path("./ebin").

stop_ruby(Ruby) ->
  ruby:stop(Ruby).

start_ruby() ->
  {ok, Ruby} = ruby:start(),
  install_encoder(Ruby),
  Ruby.

usage() ->
  io:format("usage: erruby filename\n"),
  halt(1).

