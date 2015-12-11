class Foo
  def message
    "hello world"
  end
  def hello
    puts message
  end
end
class Bar
  def message
    "hello bar"
  end
  def hello
    puts message
  end
end
f = Foo.new
f.hello
ff = Foo.new

Bar.new.hello
