#!/usr/bin/env ruby
fail_case = []
Dir.glob("rb_test/*.rb") do |fn|
  basename = fn.split('.')[0]
  outname = basename + ".out"
  unless system("./erruby #{fn} | diff #{outname} -")
    fail_case << fn
    puts "test #{fn} failed"
  end
end
if fail_case.empty?
  puts "everything pass"
else
  exit 1
end
