str = "not this"
def hello_world_method_0
  "hello world no arg"
end

def hello_world_method_1(name)
  str = name
  str
end

def hello_world_method_block(n)
  puts n
  puts yield
end

def yield_with_arg(s,x)
  yield s,x
end

yield_with_arg("yield with","arg") do |ss, xx|
  puts ss # "yield with"
  puts xx # "arg"
end

puts hello_world_method_0
puts hello_world_method_1("my name is erruby")
hello_world_method_block("arg") { "hello block" }

#hello_world_method_block(4){"hello"}.block1{"this"}.block2{"world"}
