#!/usr/bin/ruby

#
# Unit test K3 single-site execution.
# Automatically run a test on every SQL file in the DBToaster example directory
#
# Usage:
#    ./auto_test.rb [options]
#      -s (optional)        Test with only simple queries
#
#      -n NUMBER (optional) Test only a specific test number
#      -d Run distributed tests 
#      -l FILE run only a list of selected tests from a file

require 'optparse'
require 'pathname'

cur_path = File.expand_path(File.dirname(__FILE__))
examples_path = File.join(cur_path, "../external/dbtoaster/examples/queries/")
simple_dir = "simple"

simple_flag = false
test_num = nil
distributed = false
test_list_name = nil
test_list = []

# option parser
opt_parser = OptionParser.new do |opts|
	opts.banner = "Usage: auto_test.rb [options]"
	opts.separator ""
	opts.separator "Specific options:"
	opts.on("-s", "--simple", "Do only simple tests") do
			simple_flag = true 
		end
	opts.on("-n", "--testnum [NUMBER]", Integer, 
		  	"Choose a specific test") do |n|
		  test_num = n
		end
	opts.on("-d", "--dist", "Perform a distributed test") do
		  distributed = true
		end
  opts.on("-l", "--list [FILE]", String, 
          "Execute tests from a list in a file") do |file|
      test_list_name = file
    end
end

# now parse the options
opt_parser.parse!(ARGV)

# handle a test list
if test_list_name != nil then
    p = Pathname.new(test_list_name)
    (puts "Can't find list file #{test_list_name}"; exit) unless p.exist?
    File.open(test_list_name, "r").each_line do |line|
      s = /\d+/.match(line).to_s
      test_list << s.to_i
    end
end

p = Pathname.new(examples_path)
raise "Can't find path #{examples_path}" unless p.exist?

# start off with simple dir
simple_path = File.join(examples_path, simple_dir)
simple_p = Pathname.new(simple_path)
raise "Can't find path #{simple_path}" unless simple_p.exist?

all_files = []

# add all simple files
simple_p.children.each do |f|
	if f.extname == ".sql" then all_files << f end
end

# add all the other children if we're not in simple mode
if not simple_flag then
	p.children.each do |d|
		dir, last = d.split
		if last.to_s != simple_dir 
		then d.children.each do |f|
			if f.extname == ".sql" then all_files << f end
		  end 
		end
  end
end

# for debugging
#for file in all_files
	#puts file.relative_path_from(p)
#end

def short_name_of(long_name)
		dir, file = File.split(long_name)
		dir1, dir2 = File.split(dir)
		return File.join(dir2, file)
end

err_file = "./err_log.txt"

if distributed then
  test_cmd = File.join(cur_path, "./dist_test.rb")
else
  test_cmd = File.join(cur_path, "./sql_test.rb")
end

# run either one test or many tests
if test_num == nil then
	if File.exist?(err_file) then File.delete(err_file) end
	index = 1
	passed = 0
  tested = 0

	for file in all_files
    if test_list != [] and test_list.find_index(index) != nil then
      long_name = file.to_s
      short_name = short_name_of(long_name)
      print "Test #{index} (#{short_name}): "
      output = `#{test_cmd} #{long_name}` 

      if (/ERROR|FAILED/ =~ output) != nil then 
        puts "ERROR"
        # record the error in one big file
        File.open(err_file, "a") do |out|
          out << "Test #{index} #{short_name}: ERROR\n#{output}\n\n"
        end
      else puts "PASSED"; passed += 1 
      end

      tested += 1
    end

      index += 1
	end
	puts "Passed #{passed} out of #{tested} tests"
else # one test
	test_file = all_files[(test_num-1)]
	puts("Test #{test_num} (#{test_file.to_s}):")
  puts `#{test_cmd} #{test_file.to_s}`
end

