class Foo
  def setup
    @ivar = "hello"
  end
  def ivar
    @ivar
  end
end
foo = Foo.new
foo.setup
puts foo.ivar
