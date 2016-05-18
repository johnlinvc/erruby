class Foo
  def to_s
    "foo"
  end
end

class Bar < Foo
end

class Alice < Bar
  def to_s
    "i'm alice"
  end
end

puts Foo.new.to_s # "foo"
puts Bar.new.to_s # "foo"
puts Alice.new.to_s # "i'm alice"
