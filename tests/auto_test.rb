#!/usr/bin/ruby
# Automatically run a test on every SQL file we have

require 'optparse'
require 'pathname'

examples_path = "../external/dbtoaster/examples/queries/"
simple_dir = "simple"

simple_flag = false

# option parser
opt_parser = OptionParser.new do |opts|
	opts.banner = "Usage: auto_test.rb [options]"
	opts.separator ""
	opts.separator "Specific options:"
	opts.on("-s", "--[no-]simple", "Do only simple tests") do |s|
			simple_flag = s
		end
end

# now parse the options
options = opt_parser.parse!(ARGV)

p = Pathname.new(examples_path)
raise "Can't find path #{examples_path}" unless p.exist?

# start off with simple dir
simple_path = File.join(examples_path, simple_dir)
puts simple_path
simple_p = Pathname.new(simple_path)
raise "Can't find path #{simple_path}" unless simple_p.exist?
all_files = simple_p.children

# add all the other children if we're not in simple mode
if not simple_flag then
	p.children.each do |d|
		dir, last = d.split
		if last.to_s != simple_dir 
		then all_files = all_files + d.children end
	end
end

index = 1

# for debugging
#for file in all_files
	#puts file.relative_path_from(p)
#end

for file in all_files
	puts("Test #{index} (#{file.to_s}):")
  `./sql_test.rb #{file.to_s}` 
	index = index + 1
end










