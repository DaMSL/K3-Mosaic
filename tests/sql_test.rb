#!/usr/bin/ruby
# Compare the output of an SQL test using dbtoaster to K3 execution

require 'optparse'
require 'pathname'

raise "No input file" unless ARGV.length >= 1
file=File.expand_path(ARGV[0])

dbtoaster_path = "../external/dbtoaster"
dbtoaster_exe_path = File.join(dbtoaster_path, "/bin/dbtoaster")

p = Pathname.new(dbtoaster_exe_path)
raise "Can't find dbtoaster executable" unless p.exist?

path = "../bin/k3"
p = Pathname.new(path)
if not p.exist? then 
	path = "../Driver.byte"
	p = Pathname.new(path)
		if not p.exist? then
			path = "../Driver.native"
			p = Pathname.new(path)
				raise "Can't find dbtoaster file" unless p.exist?
		end
end

k3_path = path

def test_file(file, dbt_path, k3_path)
	curdir = Dir.pwd
	puts "cd #{dbt_path}"
	Dir.chdir "#{dbt_path}"
	temp_file = File.join(curdir, "temp.trace")
	m3_file = File.join(curdir, "temp.m3")
	puts "./bin/dbtoaster -d LOG-INTERPRETER-UPDATES -d LOG-INTERPRETER-TRIGGERS -d LOG-M3 #{file} > #{temp_file}"
	`./bin/dbtoaster -d LOG-INTERPRETER-UPDATES -d LOG-INTERPRETER-TRIGGERS -d LOG-M3 #{file} > #{temp_file}`
	puts "./bin/dbtoaster -l M3 #{file} > #{m3_file}"
	`./bin/dbtoaster -l M3 #{file} > #{m3_file}`
	puts "cd #{curdir}"
	Dir.chdir "#{curdir}"

	puts "#{k3_path} -p -i m3 -l k3 temp.m3 > temp2.k3"
	`#{k3_path} -p -i m3 -l k3 temp.m3 > temp2.k3 2>temp.err`

	# Check for error
	s = File.size?("./temp.err")
	if s != nil && s > 0  then 
		buf = IO.read("./temp.err")
		puts "ERROR: #{buf}"
		exit
	end

	# remove everything after "role client" from temp.k3
	File.open("temp.k3", 'w') do |out|
		out << File.open("temp2.k3").read.gsub(/role client.*/m, "")
	end

	# append the test from genmaps.rb
	puts "./genmaps.rb temp.trace >> temp.k3"
	`./genmaps.rb temp.trace >> temp.k3`

	# run the k3 driver on the input
	puts "#{k3_path} -test temp.k3"
	output = `#{k3_path} -test temp.k3`
	puts output
end

test_file(file, dbtoaster_path, k3_path)

