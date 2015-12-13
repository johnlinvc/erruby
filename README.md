# ErRuby - an implementaion of the Ruby language on Erlang
[![Build Status](https://travis-ci.org/johnlinvc/erruby.svg?branch=develop)](https://travis-ci.org/johnlinvc/erruby)
## About

ErRuby is an implementaion of the Ruby language using Erlang.

It aims to bring some concurrency features to ruby by experimenting.

It's still a work in progress. So use it at your own rist.

## Install

### Prerequisites
 
- erlang vm
- rebar2
- ruby 

To install erlang & erbar on OSX, using homebrew

	brew install erlang rebar

### Building

After getting the source of ErRuby, you have to get the gems for parser to work with bundler using:
	
	bundle install
	
 
Then get the deps of erlang modules by using:

	rebar get-deps
	
Last, compile ErRuby with:

	rebar compile
	
	
Test the binary with:

	./erruby rb_test/hello_world.rb
It should output `hello world`
## License

ErRuby is licensed to you under MIT license. See the [COPYING.txt](CCOPYING.txt) file for more details.
