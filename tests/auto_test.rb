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
# 

require 'optparse'
require 'pathname'

cur_path = File.expand_path(File.dirname(__FILE__))
examples_path = File.join(cur_path, "../external/dbtoaster/examples/queries/")
simple_dir = "simple"

simple_flag = false
test_num = nil
distributed = false

# option parser
opt_parser = OptionParser.new do |opts|
	opts.banner = "Usage: auto_test.rb [options]"
	opts.separator ""
	opts.separator "Specific options:"
	opts.on("-s", "--[no-]simple", "Do only simple tests") do |s|
			simple_flag = s
		end
	opts.on("-n", "--testnum [NUMBER]", Integer, 
					"Choose a specific test") do |n|
		  test_num = n
		end
	opts.on("-d", "--dist", "Perform a distributed test") do |n|
		  distributed = true
		end
end

# now parse the options
options = opt_parser.parse!(ARGV)

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

# run either one test or many tests
if test_num == nil then
	if File.exist?(err_file) then File.delete(err_file) end
	index = 1
	passed = 0
	for file in all_files
		long_name = file.to_s
		short_name = short_name_of(long_name)
		print "Test #{index} (#{short_name}): "
        sql_test = File.join(cur_path, "./sql_test.rb")
		output = `#{sql_test} #{long_name}` 

		if (/ERROR|FAILED/ =~ output) != nil then 
			puts "ERROR"
			# record the error in one big file
			File.open(err_file, "a") do |out|
				out << "Test #{index} #{short_name}: ERROR\n#{output}\n\n"
			end
		else puts "PASSED"; passed += 1 end

		index += 1
	end
	puts "Passed #{passed} out of #{index-1} tests"
else 
	test_file = all_files[(test_num-1)]
	puts("Test #{test_num} (#{test_file.to_s}):")
    if distributed then
	    output =  `#{cur_path}/dist_test.rb #{test_file.to_s}`
    else
        output =  `#{cur_path}/sql_test.rb #{test_file.to_s}`
    end
	puts output
end

