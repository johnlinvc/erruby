#!/usr/bin/env ruby
fail_case = []
verbose = ARGV.include?("-v")

def run_test(fn, verbose:false)
  basename = File.basename(fn,'.rb')
  outname = "rb_test/sysrb_out/#{basename}.out"
  puts "testing #{fn}" if verbose
  system("ruby #{fn} > #{outname}")
  system("./erruby #{fn} | diff #{outname} -")
end

if File.exist?(ARGV[0])
  fn = ARGV[0]
  basename = File.basename(fn,'.rb')
  test_result = run_test(fn, verbose: true)
  fail_case << fn unless test_result
else
  Dir.glob("rb_test/*.rb") do |fn|
    basename = File.basename(fn,'.rb')
    next if basename.start_with?("_")
    unless run_test(fn, verbose: verbose)
      fail_case << fn
    end
  end
end

if fail_case.empty?
  puts "everything pass"
  exit 0
else
  fail_case.each do |fn|
      puts "test #{fn} failed"
  end
  exit 1
end
