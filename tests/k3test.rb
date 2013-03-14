#!/usr/bin/ruby

require 'optparse'
require 'pathname'

options = {:k3_path=>"../k3", :dbt_path=>"./dbtoaster"}
OptionParser.new do |opts|
	opts.banner = "Usage: k3test.rb sql_file [-k k3_path] [-d db_toaster_path]"
	opts.on("-k", "--k3_path [K3PATH]", "Path to k3") do |p|
		options[:k3_path]=p
  end
	opts.on("-d", "--dbtoaster [DBTPATH]", "Path to dbtoaster") do |p|
		options[:dbt_path]=p
  end
end.parse!

raise "No input file" unless ARGV.length >= 1
options[:file]=ARGV[0]

p1 = Pathname.new(options[:k3_path])
raise "Can't find k3 file" unless p1.exist?
p2 = Pathname.new(File.join(options[:dbt_path], "/bin/dbtoaster"))
raise "Can't find dbtoaster file" unless p2.exist?

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
	`#{k3_path} -p -i m3 -l k3 temp.m3 > temp2.k3`
	File.open("temp.k3", 'w') do |out|
		out << File.open("temp2.k3").read.gsub(/role client.*/m, "")
	end
	puts "./genmaps.rb temp.trace >> temp.k3"
	`./genmaps.rb temp.trace >> temp.k3`
	puts "#{k3_path} -test temp.k3"
	output = `#{k3_path} -test temp.k3`
	puts output
end

test_file(options[:file], options[:dbt_path], options[:k3_path])

