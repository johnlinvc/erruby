class Foo
  def method
    "hello world"
  end
  def hello
    puts method
  end
  self.hello
end

f = Foo.new
f.hello
