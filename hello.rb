def hello
  puts "hello world"
end

module Test
  module_function
  def hello
    "quick brown fox"
  end
end
