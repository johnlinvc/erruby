class Foo
  def to_s
    "foo"
  end
end

class Bar < Foo
end

class Alice < Bar
end

puts Foo.new.to_s
puts Bar.new.to_s
puts Alice.new.to_s
