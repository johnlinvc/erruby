#!/usr/bin/env ruby
require 'optparse'

options = {}

OptionParser.new do |opts|
  opts.banner = "Usage: test.rb [options] test_case.rb"
  opts.on("-v", "--[no-]verbose", "Run verbosely") do |v|
    options[:verbose] = v
  end
  opts.on("-h", "--help", "Print this help") do
    puts(opts)
    exit 1
  end
end.parse!

verbose = options[:verbose]

def run_single_mri_test(fn, verbose:false)
  basename = File.basename(fn,'.rb')
  outname = "rb_test/sysrb_out/#{basename}.out"
  puts "testing #{fn}" if verbose
  system("ruby #{fn} > #{outname}")
  system("./erruby #{fn} | diff #{outname} -")
end

def run_mri_tests(verbose:)
  fail_case = []
  if ARGV[0] && File.exist?(ARGV[0])
    fn = ARGV[0]
    basename = File.basename(fn,'.rb')
    test_result = run_single_mri_test(fn, verbose: verbose)
    fail_case << fn unless test_result
  else
    Dir.glob("rb_test/*.rb") do |fn|
      basename = File.basename(fn,'.rb')
      next if basename.start_with?("_")
      unless run_single_mri_test(fn, verbose: verbose)
        fail_case << fn
      end
    end
  end
  fail_case
end

fail_case = run_mri_tests(verbose: verbose)

if fail_case.empty?
  puts "everything pass"
  exit 0
else
  fail_case.each do |fn|
      puts "test #{fn} failed"
  end
  exit 1
end
