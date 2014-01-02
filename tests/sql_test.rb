#!/usr/bin/ruby

#
# Compare the output of an SQL test using dbtoaster to K3 execution
#
# Usage:
#    ./sql_test.rb SQL_query
#

require 'optparse'
require 'pathname'

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

	# run dbtoaster to get m3 file
  puts "./bin/dbtoaster -l M3 -d PRINT-VERBOSE #{file} > #{m3_file}"
  `./bin/dbtoaster -l M3 -d PRINT-VERBOSE #{file} > #{m3_file} 2> #{err_file}`
  check_error(curdir, err_file)

	# change directory back
	puts "cd #{curdir}"
	Dir.chdir "#{curdir}"

  # convert to a k3 file to check
	puts "#{k3_path} -p -i m3 -l k3 temp.m3 > temp2.k3"
	`#{k3_path} -p -i m3 -l k3 temp.m3 > temp2.k3 2> #{err_file}`
	check_error(curdir, err_file)
  check_type_error(curdir, 'temp2.k3')

  # convert again to check for any loopback malformations
	puts "#{k3_path} -p -i k3 -l k3 temp2.k3 > temp3.k3"
	`#{k3_path} -p -i k3 -l k3 temp2.k3 > temp3.k3 2> #{err_file}`
	check_error(curdir, err_file)
  check_type_error(curdir, 'temp3.k3')

  # convert to a test
	puts "#{k3_path} -p -i m3 -l k3test -trace #{trace_file} temp.m3 > temp.k3"
	`#{k3_path} -p -i m3 -l k3test -trace #{trace_file} temp.m3 > temp.k3 2> #{err_file}`
	check_error(curdir, err_file)
  check_type_error(curdir, 'temp.k3')

	# run the k3 driver on the input to get test results
	puts "#{k3_path} --test temp.k3"
	output = `#{k3_path} --test temp.k3 2> #{err_file}`
	check_error(curdir, err_file)
	puts output
end

test_file(file, dbtoaster_path, k3_path)

