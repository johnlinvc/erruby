#!/usr/bin/env ruby

require 'parser/current'

src  = ARGF.read

ast = Parser::CurrentRuby.parse(src)

p ast

