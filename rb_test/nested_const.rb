class Foo
  Bar = "hello nested"
  class Alice
  end
end
Bob = "outside bob"
puts "Foo::Bar"
puts Foo::Bar
Foo::Alice::Bob = "inside Bob"
puts Foo::Alice::Bob
puts Bob
