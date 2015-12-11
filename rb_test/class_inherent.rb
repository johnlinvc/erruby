class Foo
  def to_s
    "foo"
  end
end

class Bar
  def to_s
    "bar"
  end
end

puts Foo.new.to_s
puts Bar.new.to_s
