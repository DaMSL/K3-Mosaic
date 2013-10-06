#!/usr/bin/env ruby
require "./#{File.dirname($0)}/util.rb"
require "./#{File.dirname($0)}/db_parser.rb"
require 'tempfile'

script = ARGV.shift;

dbt_opts = [];
if /\.m3$/ =~ script then
  dbt_opts.push("-i m3");
  sql_script = ARGV.shift;
else
  sql_script = script;  
end

script_data = File.open(sql_script).readlines;

$tables = Hash.new { |h,k|p k; h[k] = Hash.new(0) };

$pg_script =
  script_data.
    map {|i| i.sub(/--.*/, "")}.
    map {|i| i.chomp }.join(" ").
    sub(/\/\*.*\*\//, "").
    split(/ *; */).
    map do |cmd| 
      case cmd.upcase 
        when /CREATE STREAM ([^(]+)(.*) FROM/ then 
          table = $1.downcase.chomp(" "); 
          schema = $2; 
          $tables[table];
          "CREATE TEMPORARY TABLE #{table}#{schema};\n"+
          "COPY #{table} FROM '@@#{table.downcase}@@' DELIMITER ',';"
        when /SELECT */ then "#{cmd};"
        else raise "Error: Unexpected statement in SQL file \n#{cmd}"
      end
    end.
    join("\n");

dbt_cmd = 
  "./bin/dbtoaster -r -d step-interpreter -d log-interpreter-updates #{ARGV.join(" ")} #{dbt_opts.join(" ")} #{script}";

def update_rel(op, rel, key)
  rel = rel.downcase;
  if op == "-" then
    if $tables[rel][key] = 1 then $tables[rel].delete(key)
    elsif $tables[rel][key] < 1 then puts "ERROR: invalid delete"; exit -1;
    else $tables[rel][key] = $tables[rel][key] - 1;
    end
  else
    $tables[rel][key] = $tables[rel][key] + 1;
  end
end

def compare_results(dbtoaster, postgres = correct_results)
  (dbtoaster.keys + postgres.keys).uniq.each do |k|
     
     puts("MILOS" +
        (dbtoaster.keys + postgres.keys).uniq.map do |k| 
          "[#{k.join(", ")}] / #{postgres[k]} / #{dbtoaster[k]}"
        end.join("\n")
      );

     
    if dbtoaster[k].to_f != postgres[k].to_f then
      puts "============ Value Mismatch ============";
      puts "--- STATE ---";
      p $tables;
      puts "--- Key/EXPECTED/DBToaster ---";
      puts(
        (dbtoaster.keys + postgres.keys).uniq.map do |k| 
          "[#{k.join(", ")}] / #{postgres[k]} / #{dbtoaster[k]}"
        end.join("\n")
      );
      exit -1;
    end
  end
end

def correct_results()
  pg_script = $pg_script;
  rel_files = $tables.map do |rname, vals|
    f = Tempfile.new(rname)
    vals.each do |row,arity|
      (0...arity).each { f.puts(row.join(",")) };
    end
    f.flush
    pg_script = pg_script.sub("@@#{rname.downcase}@@", f.path);
    f
  end
  p pg_script;
  correct = 
    IO.popen("psql dbtoaster -t", "r+") do |psql|
      psql.puts(pg_script);
      psql.close_write;
      results = psql.readlines
      p results;
      results.
        delete_if { |row| (not (row.include? "|")) && 
                          (row.to_i.to_s != row.chomp.sub(/^ */,"")) }.
        map do |row| 
          row.chomp.split(/ *\| */).map { |i| i.to_f }
        end.
        delete_if { |i| i == ""; }.
        map { |row| [(k = row.clone), (k.pop)] }.
        map { |k,v| [k.map {|i| i.to_i}, v] }.
        to_h
    end
  rel_files.each { |f| f.close! }
  correct;
end


IO.popen(dbt_cmd, "r+") do |f|
  loop do
    line = f.gets;
    exit 0 if line.nil?;
    puts line;
    case line
      when nil then 
      when /^(\+|-)([a-zA-Z]+)\[([\-0-9.; ]*)\]/ then 
        update_rel($1, $2, $3.split(/; */).map {|i| i.to_f});
        
      when /QUERY22->/ then
        results = OcamlDB.new(line)["QUERY22"];
        results = {[] => results} unless results.is_a? Hash;
        results = results.to_a.
          map { |k,v| [k.map{|i| i.to_i}, v]}.
          to_h
        compare_results(results)
        f.puts "\n";
    end
  end
end
