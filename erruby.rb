#!/usr/bin/env ruby

require 'rubygems'
require 'bundler/setup'
require 'erlport/ast_mapping'

def parse(src)
  ErlPort::AstMapping.parse(src)
end

def install_encoder
  ErlPort::AstMapping.install_encoder
end

if __FILE__ == $0
  p parse("[1,2,3]".each_char.map(&:ord))
end
