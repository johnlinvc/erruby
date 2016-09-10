[true, false]
[nil, nil]
["a","b","c"]
[1,2].map{|x| puts(x.to_s)}
puts "mul"
([1,2,3] * 3).map{|x| puts x.to_s}
#a = 0
#[1,2,3].map do |x|
  #a = a + x
#end
#puts a
puts ["a","b","c"].at(1)
puts ["a","b","c"].first
puts ["a","b","c"].last

puts [false].empty?.to_s
puts [].empty?.to_s
puts [1, 2, 3].length.to_s
puts [1, 2, 3].size.to_s
ary = ["a","b"]
ary.concat ["c", "d"]
puts ary.last
ary2 = ary + ["e", "f"]
puts ary2.last
puts ary.last
