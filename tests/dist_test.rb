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
$force_correctives = false
$order_file = ""

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
    opts.on("-f", "--force_correctives", 
            "Force correctives") do
        $force_correctives = true
        end
    opts.on("-o", "--order [STRING]", String,
            "Use an order file instead of creating a trace") do |s|
        $order_file = s
        end
end

# now parse the options
opt_parser.parse!(ARGV)

raise "No input file" unless ARGV.length >= 1
file=File.expand_path(ARGV[0])

$cur_path = File.expand_path(File.dirname(__FILE__))
$dbtoaster = "dbtoaster_release"
dbtoaster_path = File.join($cur_path, "../external/dbtoaster")
dbtoaster_exe_path = File.join(dbtoaster_path, "/bin/#{$dbtoaster}")

$shuffle_cmd = if $shuffle then "--shuffle" else "" end
$force_cmd = if $force_correctives then "--force" else "" end

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
    puts "./bin/#{$dbtoaster} -d LOG-INTERPRETER-UPDATES -d LOG-INTERPRETER-TRIGGERS -d LOG-M3 #{file} > #{trace_file}"
    `./bin/#{$dbtoaster} -d PRINT-VERBOSE -d LOG-INTERPRETER-UPDATES -d LOG-INTERPRETER-TRIGGERS -d LOG-M3 #{file} > #{trace_file} 2> #{err_file}`
    check_error(curdir, err_file)

	# run dbtoaster to get m3 file with distributed portion
    puts "./bin/#{$dbtoaster} -l distm3 -d PRINT-VERBOSE #{file} > #{m3_file}"
    `./bin/#{$dbtoaster} -l distm3 -d PRINT-VERBOSE #{file} > #{m3_file} 2> #{err_file}`
    check_error(curdir, err_file)

	# change directory back
	puts "cd #{curdir}"
	Dir.chdir "#{curdir}"

    # create a k3 file for comparison's sake
	puts "#{k3_path} -p -i m3 -l k3 temp.m3 > temp.k3"
	`#{k3_path} -p -i m3 -l k3 temp.m3 > temp.k3 2> #{err_file}`

    # string for k3 distributed file creation: either use a trace file or an order file
    create_str = if $order_file=="" then "--trace #{trace_file}" else "--order #{$order_file}" end

    # create a k3 distributed file (without a partition map)
	puts "#{k3_path} -p --lambda -i m3 -l k3disttest temp.m3 #{create_str} #{$force_cmd} > temp.k3dist"
	`#{k3_path} -p --lambda -i m3 -l k3disttest temp.m3 #{create_str} #{$force_cmd} > temp.k3dist 2> #{err_file}`
	check_error(curdir, err_file)
    check_type_error(curdir, 'temp.k3dist')
    
    if $num_nodes > 1 then
      # create a partition map
      puts "#{$part_path} temp.k3dist -n #{$num_nodes} > temp.part"
      output = `#{$part_path} temp.k3dist -n #{$num_nodes} > temp.part`
      
      # create another k3 distributed file (with partition map)
      puts "#{k3_path} -p --lambda -i m3 -l k3disttest temp.m3 #{create_str} -m temp.part #{$force_cmd} > temp.k3dist"
      `#{k3_path} -p --lambda -i m3 -l k3disttest temp.m3 #{create_str} -m temp.part #{$force_cmd} > temp.k3dist 2> #{err_file}`
      check_error(curdir, err_file)
      check_type_error(curdir, 'temp.k3dist')
    end
    
    # create a k3new distributed file (for convenience)
	puts "#{k3_path} -p -i k3 -l k3new temp.k3dist > temp.k3new"
	`#{k3_path} -p -i k3 -l k3new temp.k3dist > temp.k3new 2> #{err_file}`
	check_error(curdir, err_file)

    # create node list
    node_list = Array.new($num_nodes) do |i|
        "#{i}.#{i}.#{i}.#{i}:10/node"
    end

    peer_str = "-n localhost:10000/switch"
    node_list.each do |ip|
        peer_str += ",#{ip}"
    end

	# run the k3 driver on the input
	puts "#{k3_path} --test #{peer_str} -q #{$q_type} #{$shuffle_cmd} temp.k3dist"
	output = `#{k3_path} --test #{peer_str} -q #{$q_type} #{$shuffle_cmd} temp.k3dist 2> #{err_file}`
	check_error(curdir, err_file)
	puts output
    exit
end

test_file(file, dbtoaster_path, k3_path)

