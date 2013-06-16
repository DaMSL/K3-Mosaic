#!/usr/bin/ruby

#
# Test a distributed SQL query
#
# Usage:
#    ./dist_test.rb SQL_query
#

require 'optparse'
require 'pathname'

$num_peers = 1

# option parser
opt_parser = OptionParser.new do |opts|
	opts.banner = "Usage: dist_test.rb [options]"
	opts.separator ""
	opts.separator "Specific options:"
    opts.on("-p", "--peers [NUMBER]", Integer,
            "Choose a number of peers") do |n|
        $num_peers = n
        end
end

# now parse the options
opt_parser.parse!(ARGV)

raise "No input file" unless ARGV.length >= 1
file=File.expand_path(ARGV[0])

$cur_path = File.expand_path(File.dirname(__FILE__))
dbtoaster_path = File.join($cur_path, "../external/dbtoaster")
dbtoaster_exe_path = File.join(dbtoaster_path, "/bin/dbtoaster")

p = Pathname.new(dbtoaster_exe_path)
raise "Can't find dbtoaster executable" unless p.exist?

path = File.join($cur_path, "../bin/k3")
p = Pathname.new(path)
raise "Can't find dbtoaster file" unless p.exist?

k3_path = path

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

	temp_file = File.join(curdir, "temp.trace")
	m3_file = File.join(curdir, "temp.m3")
	err_file = File.join(curdir, "temp.err")

    # run dbtoaster to get interpreted updates
    puts "./bin/dbtoaster -d LOG-INTERPRETER-UPDATES -d LOG-INTERPRETER-TRIGGERS -d LOG-M3 #{file} > #{temp_file}"
    `./bin/dbtoaster -d PRINT-VERBOSE -d LOG-INTERPRETER-UPDATES -d LOG-INTERPRETER-TRIGGERS -d LOG-M3 #{file} > #{temp_file} 2> #{err_file}`
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

    # create a k3 distributed file
	puts "#{k3_path} -p -i m3 -l k3dist temp.m3 > temp2.k3dist"
	`#{k3_path} -p -i m3 -l k3dist temp.m3 > temp2.k3dist 2> #{err_file}`
	check_error(curdir, err_file)
    check_type_error(curdir, 'temp2.k3dist')

    # parse the distributed file
	puts "#{k3_path} -p -i k3 temp2.k3dist > temp3.k3dist"
	`#{k3_path} -p -i k3 -l k3 temp2.k3dist > temp3.k3dist 2> #{err_file}`
	check_error(curdir, err_file)
    check_type_error(curdir, 'temp3.k3dist')

    # compare the parsed file to the original file via AST
	puts "#{k3_path} -p -i k3 -l k3ast temp2.k3dist > temp2.k3ast"
	`#{k3_path} -p -i k3 -l k3ast temp2.k3dist > temp2.k3ast 2> #{err_file}`
	check_error(curdir, err_file)
    check_type_error(curdir, 'temp2.k3ast')
	puts "#{k3_path} -p -i k3 -l k3ast temp3.k3dist > temp3.k3ast"
	`#{k3_path} -p -i k3 -l k3ast temp3.k3dist > temp3.k3ast 2> #{err_file}`
	check_error(curdir, err_file)
    check_type_error(curdir, 'temp3.k3ast')
    check_file_match(curdir, 'temp2.k3ast', 'temp3.k3ast')
    File.unlink('temp2.k3ast')
    File.unlink('temp3.k3ast')
    File.unlink('temp3.k3dist')

	# remove everything after "role client" from temp.k3
	File.open("temp.k3dist", 'w') do |out|
		out << File.open("temp2.k3dist").read.gsub(/role client.*/m, "")
	end

	# append the test from genmaps.rb
    genmaps = File.join($cur_path, "./genmaps.rb")
	puts "#{genmaps} --distrib temp.trace >> temp.k3dist"
	`#{genmaps} --distrib temp.trace >> temp.k3dist`

    # create peer list
    peer_list = Array.new($num_peers - 1) do |i|
        "#{i}.#{i}.#{i}.#{i}:10"
    end

    peer_str = "-n localhost:10000/switch"
    peer_list.each do |ip|
        peer_str += ",#{ip}"
    end

	# run the k3 driver on the input
	puts "#{k3_path} -eval #{peer_str} temp.k3dist"
	output = `#{k3_path} -eval #{peer_str} temp.k3dist 2> #{err_file}`
	check_error(curdir, err_file)
	puts output
    exit
end

test_file(file, dbtoaster_path, k3_path)

