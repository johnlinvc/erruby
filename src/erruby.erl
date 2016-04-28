-module(erruby).
-export([eruby/1, start_ruby/0, stop_ruby/1, parse_ast/2, main/1]).

opt_spec_list() ->
  [
   {debug, $d, "debug", {integer, 0}, "Verbose level for debugging"},
   {verbose, $v, "verbose", undefined, "print version number and enter verbose mode"},
   {help, $h, "help", undefined, "Show this help"}
  ].

handle_opts({debug, DebugLevel}) ->
  erruby_debug:set_debug_level(DebugLevel);
handle_opts(help ) ->
  show_help();
handle_opts(verbose) ->
  io:format("erruby 0.1.0~n"),
  erruby_debug:set_debug_level(2);
handle_opts(_Opts) ->
  ok.

main(Args) ->
  add_lib_path(),
  erruby_debug:start_link(0),
  {ok, {Opts, Extra}} = getopt(Args),
  lists:foreach(fun handle_opts/1, Opts),
  [SrcFileName | RubyArgs] = Extra,
  try
    erruby_debug:debug_2("input file name ~s\n", [SrcFileName]),
    erruby_debug:debug_2("input args ~s\n", [RubyArgs]),
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
  erruby_vm:eval_file(Ast, SrcFileName).

getopt(Args) ->
  getopt:parse(opt_spec_list(), Args).

show_help() ->
  getopt:usage(opt_spec_list(), "erruby", "[programfile] [arguments]"),
  halt(1).


install_encoder(Ruby) ->
  ruby:call(Ruby, erruby_rb_path() , 'install_encoder',[]).

erruby_path() ->
  filename:dirname(escript:script_name()).

relative_path(Path) ->
  erruby_path() ++ Path.

erruby_rb_path() ->
  list_to_atom(relative_path("/../rb_src/erruby.rb")).

parse_ast(Ruby, String) ->
  ruby:call(Ruby, erruby_rb_path(),'parse', [String]).

add_lib_path() ->
  code:add_path(relative_path("/../deps/erlport/ebin")),
  code:add_path(relative_path("/../deps/getopt/ebin")),
  code:add_path(erruby_path()).

stop_ruby(Ruby) ->
  ruby:stop(Ruby).

start_ruby() ->
  {ok, Ruby} = ruby:start(),
  install_encoder(Ruby),
  Ruby.

