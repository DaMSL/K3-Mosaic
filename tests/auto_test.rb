#!/opt/local/bin/ruby1.9

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

dir_filter = ""
test_num = nil
test_name = nil
distributed = false
test_list_name = nil
test_list = []
$num_nodes = 1
$q_type = "global"
$shuffle = false

# option parser
opt_parser = OptionParser.new do |opts|
	opts.banner = "Usage: auto_test.rb [options]"
	opts.separator ""
	opts.separator "Specific options:"
	opts.on("-s", "--simple", "Do only simple tests") do
			dir_filter = "simple" 
		end
	opts.on("-l", "--filter [SIMPLE]", String, "Run only specific tests") do |s|
			dir_filter = s
		end
	opts.on("-f", "--file [NAME]", String, "Run a specific test file") do |s|
			test_name = s
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
    opts.on("-p", "--nodes [NUMBER]", Integer,
            "Choose a number of nodes") do |n|
        $num_nodes = n
        end
    opts.on("-q", "--queue [STRING]", String,
            "Select type of queue: global/trigger/node") do |s|
        $q_type = s
        end
    opts.on("-x", "--shuffle", 
            "Shuffle the queues") do
        $shuffle = true
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

simple_files = []
all_files = []

# start off with simple dir so it appears first in our list
simple_path = File.join(examples_path, "simple")
simple_p = Pathname.new(simple_path)
raise "Can't find path #{simple_path}" unless simple_p.exist?

# add all simple files so they're first in our list
simple_p.children.each do |f|
  if f.extname == ".sql" then simple_files << ["simple", f.basename, f] end
end
simple_files.sort_by! do |_, name, _| name end

# Add all children but simple path files
p.children.each do |d|
  dir, last = d.split
  if last.to_s != "simple" then 
    d.children.each do |f|
        if f.extname == ".sql" then all_files << [last, f.basename, f] end
    end 
  end
end

# Sort the array
all_files.sort_by! do |last, fname, f| fname end
all_files.sort_by! do |last, fname, f| last end

# Combine with simple array
all_files = simple_files.concat(all_files)
all_files.map! do |_, _, f| f end

err_file = "./err_log.txt"
node_cmd = ""
$shuffle_cmd = if $shuffle then "--shuffle" else "" end

if distributed then
  test_cmd = File.join(cur_path, "./dist_test.rb")
  node_cmd = "-p #{$num_nodes} -q #{$q_type}"
else
  test_cmd = File.join(cur_path, "./sql_test.rb")
end

# run either one test or many tests
if test_num == nil then
	if File.exist?(err_file) then File.delete(err_file) end
	index = 1
	passed = 0
    tested = 0

	for file, last_dir in all_files
    if test_list == [] or (test_list.find_index(index) != nil) then
      long_name = file.to_s
      dir, file = File.split(long_name)
      first, last = File.split(dir)
      short_name = File.join(last, file)
      # Skip non-matching files
      if dir_filter != "" and dir_filter != last then 
        index += 1
        next 
      end
      print "Test #{index} (#{short_name}): "

      output = `#{test_cmd} #{node_cmd} #{$shuffle_cmd} #{long_name}` 

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
	file = all_files[(test_num-1)]
	puts("Test #{test_num} (#{file.to_s}):")
    puts `#{test_cmd} #{node_cmd} #{$shuffle_cmd} #{file.to_s}`
end

