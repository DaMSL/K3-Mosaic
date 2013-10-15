#!/usr/bin/env ruby

#
# Test a distributed SQL query
#
# Usage:
#    ./dist_test.rb SQL_query
#

require 'optparse'
require 'pathname'

$num_nodes = 1
$q_type = "global"
$shuffle = false

# option parser
opt_parser = OptionParser.new do |opts|
	opts.banner = "Usage: dist_test.rb [options]"
	opts.separator ""
	opts.separator "Specific options:"
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

raise "No input file" unless ARGV.length >= 1
file=File.expand_path(ARGV[0])

$cur_path = File.expand_path(File.dirname(__FILE__))
dbtoaster_path = File.join($cur_path, "../external/dbtoaster")
dbtoaster_exe_path = File.join(dbtoaster_path, "/bin/dbtoaster")

$shuffle_cmd = if $shuffle then "-shuffle" else "" end

p = Pathname.new(dbtoaster_exe_path)
raise "Can't find dbtoaster executable" unless p.exist?

k3_path = File.join($cur_path, "../bin/k3")
p = Pathname.new(k3_path)
raise "Can't find dbtoaster file" unless p.exist?

$part_path = File.join($cur_path, "../bin/partmap_tool")
p = Pathname.new($part_path)
raise "Can't find partmap_tool file" unless p.exist?

def check_error(dir, err_file)
	# Check for error
	s = File.size?(err_file)
	if s != nil && s > 0  then 
		buf = IO.read(err_file)
		puts "ERROR: #{buf}"
		Dir.chdir dir
		exit
	end
end

def check_type_error(dir, data_file)
    buf = IO.read(data_file)
    res = buf.match(/^Error.*$/)
    if res != nil then
        puts "ERROR: #{res}"
        Dir.chdir dir
        exit
    end
end

def check_file_match(dir, file1, file2)
    out = `cmp #{file1} #{file2}`
    if out == "" then 
        puts "PASSED AST comparison"
    else
        puts "ERROR: AST mismatch while generating files"
        Dir.chdir dir
        exit
    end
end

def test_file(file, dbt_path, k3_path)
	curdir = Dir.pwd

	#change to dbtoaster path (dbtoaster needs it)
	puts "cd #{dbt_path}"
	Dir.chdir dbt_path

	trace_file = File.join(curdir, "temp.trace")
	m3_file = File.join(curdir, "temp.m3")
	err_file = File.join(curdir, "temp.err")

    # run dbtoaster to get interpreted updates
    puts "./bin/dbtoaster -d LOG-INTERPRETER-UPDATES -d LOG-INTERPRETER-TRIGGERS -d LOG-M3 #{file} > #{trace_file}"
    `./bin/dbtoaster -d PRINT-VERBOSE -d LOG-INTERPRETER-UPDATES -d LOG-INTERPRETER-TRIGGERS -d LOG-M3 #{file} > #{trace_file} 2> #{err_file}`
    check_error(curdir, err_file)

	# run dbtoaster to get m3 file with distributed portion
    puts "./bin/dbtoaster -l distm3 -d PRINT-VERBOSE #{file} > #{m3_file}"
    `./bin/dbtoaster -l distm3 -d PRINT-VERBOSE #{file} > #{m3_file} 2> #{err_file}`
    check_error(curdir, err_file)

	# change directory back
	puts "cd #{curdir}"
	Dir.chdir "#{curdir}"

    # create a k3 file for comparison's sake
	puts "#{k3_path} -p -i m3 -l k3 temp.m3 > temp.k3"
	`#{k3_path} -p -i m3 -l k3 temp.m3 > temp.k3 2> #{err_file}`

    # create a parition map
    puts "#{$part_path} temp.trace -n #{$num_nodes} > temp.part"
    output = `#{$part_path} temp.trace -n #{$num_nodes} > temp.part`
    part_str = if $num_nodes > 1 then "-m temp.part" else "" end

    # create a k3 distributed file
	puts "#{k3_path} -p -i m3 -l k3disttest  temp.m3 -trace #{trace_file} #{part_str} > temp.k3dist"
	`#{k3_path} -p -i m3 -l k3disttest temp.m3 -trace #{trace_file} #{part_str} > temp.k3dist 2> #{err_file}`
	check_error(curdir, err_file)
    check_type_error(curdir, 'temp.k3dist')

    # create node list
    node_list = Array.new($num_nodes) do |i|
        "#{i}.#{i}.#{i}.#{i}:10/node"
    end

    peer_str = "-n localhost:10000/switch"
    node_list.each do |ip|
        peer_str += ",#{ip}"
    end

	# run the k3 driver on the input
	puts "#{k3_path} -test #{peer_str} -q #{$q_type} #{$shuffle_cmd} temp.k3dist"
	output = `#{k3_path} -test #{peer_str} -q #{$q_type} #{$shuffle_cmd} temp.k3dist 2> #{err_file}`
	check_error(curdir, err_file)
	puts output
    exit
end

test_file(file, dbtoaster_path, k3_path)

