#!/usr/bin/ruby
# ./bin/dbtoaster -d LOG-INTERPRETER-UPDATES -d LOG-INTERPRETER-TRIGGERS -d LOG-M3

#K3Route, K3Ring
require 'optparse'

$options = {}
opt_parser = OptionParser.new do |opts|
    opts.banner = "Usage: genmaps.rb [options] file"
    opts.separator ""
    opts.separator "Specific options:"
    opts.on("-d", "--distrib", "Create output for distributed files") do
        $options[:distributed] = true
    end
end

opt_parser.parse!(ARGV)

class Array
  def to_h
    ret = Hash.new;
    each { |k,v| ret[k] = v; }
    return ret;
  end

  def unzip
    ret = Array.new;
    each_index do |i|
      ret.push Array.new(i) while ret.length < self[i].length
      ret.each_index do |j|
        ret[j][i] = self[i][j]
      end
    end
    return ret;
  end
end

class SingletonMap 
  attr_reader :name;
  
  def initialize(mapname, maptype)
    @name = mapname;
    @type = maptype;
    @val = 0;
  end
  
  def set(ivars, ovars, val)
    @val = val;
  end

	def del(ivars, ovars)
		@val = 0
	end
  
  def to_s
    "#{@name}-Singleton"
  end
  
  def val_s(ts = nil)
    "[#{(ts+[@val]).join("; ")}]"
  end
  
  def val_s(ts = nil)
    ts = ts.nil? ? [] : ["(#{ts.join(", ")})"];
    "(#{(ts+[@val]).join(", ")})"
  end
end

class OutputMap 
  attr_reader :name;
  
  def initialize(mapname, maptype, ovars, otypes)
    @name = mapname;
    @type = maptype;
    @ovars = ovars;
    @otypes = otypes;
    @val = Hash.new(0);
  end
  
  def set(ivars, ovars, val)
    @val[ovars] = val;
  end

	def del(ivars, ovars)
		@val.delete(ovars)
	end
  
  def to_s
    "#{@name}[#{@ovars.join(", ")}]"
  end
  
  def val_s(ts = nil)
    ts = ts.nil? ? [] : ["(#{ts.join(", ")})"];
    @val.map { |k,v| "(#{(ts+k+[v]).join(", ")})" }.join("; ")
  end
end

$next_evt_id = 0;

class RelEvent
  attr_reader :effects, :id;

  def initialize(op, relname, vals)
    @op = (op == "+") ? "insert" : "delete";
    @relname = relname
    @vals = vals;
    @effects = Hash.new { |h,k| h[k] = Array.new };
    @id = $next_evt_id;
    $next_evt_id += 1;
  end

  def add_effect(mapn, ivars, ovars, val)
    @effects[mapn].push([:add, ivars, ovars, val]);
  end

	def del_effect(mapn, ivars, ovars)
		@effects[mapn].push([:del, ivars, ovars, 0]);
	end

  def to_s
    "#{@op}_#{@relname}"
  end
  
  def dispatch_s(last)
		send = "send(#{to_s}, me, #{@vals.join(", ")})"
		semi = ";"
		if last then send
		else send + semi
		end
  end
end

$maps = Hash.new;
$events = Array.new;
$last_event = nil;
$use_timestamps = false;
$partition_map = nil;
$sys_ready = false;

raise "Missing input file" unless ARGV.length == 1;

File.open(ARGV[0]) do |f|
  f.each do |l|
    case l
      when /DECLARE MAP ([^(]+)\((int|float)\)\[([^\]]*)\]\[([^\]]*)\]/ then
        mapname = $1; maptype = $2; ivars = $3; ovars = $4;
        raise "Input Vars Unsupported" unless ivars == "";
        if(ovars == "") then
          $maps[mapname] = SingletonMap.new(mapname, maptype);
        else
          ovars,otypes = ovars.split(/, */).map { |v| v.split(/:/) }.unzip
          $maps[mapname] = OutputMap.new(mapname, maptype, ovars, otypes);
        end
			when /ON SYSTEM READY \{\n[^\}]*\}/ then
				$sys_ready = true
      when /ON (\+|-) ([^(]+)\([^)]*\) <- \[([^\]]*)\]/ then
        op = $1; relname = $2; vals = $3.split(/; */);
        $last_event = RelEvent.new(op, relname, vals);
        $events.push $last_event;
      when /UPDATE '([^']*)'\[([^\]]*)\]\[([^\]]*)\] := (.*)$/ then
        mapname = $1; ivars = $2; ovars = $3; val = $4;
        ivars = (ivars == "-" ? [] : ivars.split(/; /));
        ovars = (ovars == "-" ? [] : ovars.split(/; /));
        if $last_event != nil 
				then $last_event.add_effect(mapname, ivars, ovars, val) end
      when /REMOVE '([^']*)'\[([^\]]*)\]\[([^\]]*)\]/ then
        mapname = $1; ivars = $2; ovars = $3;
        ivars = (ivars == "-" ? [] : ivars.split(/; /));
        ovars = (ovars == "-" ? [] : ovars.split(/; /));
        if $last_event != nil 
				then $last_event.del_effect(mapname, ivars, ovars) end
    end
  end
end

puts "trigger go(id : int) {} = do {"
if $sys_ready then puts "  send(system_ready_event, me, 1);" end

len = $events.length

$events.each_index do |i|
	if i >= len-1 then puts "  #{$events[i].dispatch_s(true)}";
	else puts "  #{$events[i].dispatch_s(false)}"
  end
end
puts "}"

if $options[:distributed] then
  puts ""
  puts "role switch {"
  puts "  source s_on_init : int = stream([1])"
  puts "  bind s_on_init -> on_init"
  puts "  consume s_on_init"
  puts "  source s1 : int = stream([1])"
  puts "  bind s1 -> go"
  puts "  consume s1"
  puts "}"
  puts ""
  puts "role node {"
  puts "  source s_on_init : int = stream([1])"
  puts "  bind s_on_init -> on_init"
  puts "  consume s_on_init"
  puts "}"
  puts ""
  puts "default role node"
  puts ""
else # single-site
  puts ""
  puts "role test {"
  puts "  source s1 : int = stream([1])"
  puts "  bind s1 -> go"
  puts "  consume s1"
  puts "}"
  puts ""
  puts "default role test"
  puts ""
end

def dump_map(mapn)
  map = $maps[mapn]
  sep = "";
  puts "#{mapn} = {"
  $events.each do |evt|
    evt.effects.fetch(mapn, []).each do |action,a,b,c| 
			case action
			when :add then
				map.set(a,b,c)
			when :del then
				map.del(a,b)
			end
		end
    if $use_timestamps then
      puts(sep + map.val_s([0, 0, evt.id]));
      sep = "; ";
    end
  end
  unless $use_timestamps then
    puts(map.val_s);
  end
  puts "}"
end

if $partition_map.nil? then
  if $options[:distributed] then puts "network expected" else puts "expected" end
  sep = ""
  $maps.each { |mapn,m| puts sep; dump_map(mapn); sep = ", "; }
else
  raise "Partition maps unsupported";
end

#$maps.values.each { |m| puts m.to_s }
#$events.each { |e| puts e.to_s }
