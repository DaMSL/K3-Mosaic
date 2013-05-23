#!/usr/bin/ruby

#
# Usage:
#    ./m3_test.rb m3_file
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

k3_path = File.join($cur_path, "../bin/k3")
p = Pathname.new(k3_path)
raise "Can't find k3 exe file" unless p.exist?

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

def test_file(file, dbt_path, k3_path)
	curdir = Dir.pwd

	#change to dbtoaster path (dbtoaster needs it)
	puts "cd #{dbt_path}"
	Dir.chdir dbt_path

	temp_file = File.join(curdir, "temp.trace")
	err_file = File.join(curdir, "temp.err")

    # run dbtoaster to get interpreted updates
    puts "./bin/dbtoaster -d LOG-INTERPRETER-UPDATES -d LOG-INTERPRETER-TRIGGERS -d LOG-M3 #{file} > #{temp_file}"
    `./bin/dbtoaster -d PRINT-VERBOSE -d LOG-INTERPRETER-UPDATES -d LOG-INTERPRETER-TRIGGERS -d LOG-M3 #{file} > #{temp_file} 2> #{err_file}`
    check_error(curdir, err_file)

	# change directory back
	puts "cd #{curdir}"
	Dir.chdir "#{curdir}"

	puts "#{k3_path} -p -i m3 -l k3 #{file} > temp2.k3"
	`#{k3_path} -p -i m3 -l k3 #{file} > temp2.k3 2> #{err_file}`
	check_error(curdir, err_file)

	# remove everything after "role client" from temp.k3
	File.open("temp.k3", 'w') do |out|
		out << File.open("temp2.k3").read.gsub(/role client.*/m, "")
	end

	# append the test from genmaps.rb
    genmaps = File.join($cur_path, "./genmaps.rb")
	puts "#{genmaps} temp.trace >> temp.k3"
	`#{genmaps} temp.trace >> temp.k3`

	# run the k3 driver on the input
	puts "#{k3_path} -test temp.k3"
	output = `#{k3_path} -test temp.k3 2> #{err_file}`
	check_error(curdir, err_file)
	puts output
end

test_file(file, dbtoaster_path, k3_path)

