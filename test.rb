#!/usr/bin/env ruby
fail_case = []
verbose = ARGV[0] == "-v"
Dir.glob("rb_test/*.rb") do |fn|
  basename = File.basename(fn,'.rb')
  next if basename.start_with?("_")
  outname = "rb_test/sysrb_out/#{basename}.out"
  puts "testing #{fn}" if verbose
  system("ruby #{fn} > #{outname}")
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
