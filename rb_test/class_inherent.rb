class Foo
  def to_s
    "foo"
  end
end

class Bar < Foo
end

puts Foo.new.to_s
puts Bar.new.to_s
