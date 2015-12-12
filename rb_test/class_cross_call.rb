class Foo
  def msg
    "hello"
  end
end

class Bar
  def say
    puts Foo.new.msg
  end
end

Bar.new.say
