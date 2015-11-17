str = "not this"
def hello_world_method_0
  "hello world no arg"
end
def hello_world_method_1(name)
  str = name
  str
end

def hello_world_method_block(n)
  "FIXME remove me"
  puts "im here"
  yield
end

puts hello_world_method_0
puts hello_world_method_1("my name is erruby")
hello_world_method_block("arg") { "hello block" }

#hello_world_method_block(4){"hello"}.block1{"this"}.block2{"world"}
