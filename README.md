# ErRuby - an implementaion of the Ruby language on Erlang

## About

ErRuby is an implementaion of the Ruby language using Erlang.

It aims to bring some concurrency features to ruby by experimenting.

It's still a work in progress. So use it at your own rist.

## Install

Prerequisites:
 
- erlang vm
- rebar2
- ruby 

After getting the source of ErRuby, you have to get the gems for parser to work with bundler using:
	
	bundle install
	
 
Then get the deps of erlang modules by using:

	rebar get-deps
	
Last, compile ErRuby with:

	rebar compile
	
## License

ErRuby is licensed to you under MIT license. See the [COPYING.txt](CCOPYING.txt) file for more details.