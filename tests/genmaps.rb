#!/usr/bin/ruby
# ./bin/dbtoaster -d LOG-INTERPRETER-UPDATES -d LOG-INTERPRETER-TRIGGERS -d LOG-M3

#K3Route, K3Ring

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
    @effects[mapn].push([ivars, ovars, val]);
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
      when /ON (\+|-) ([^(]+)\([^)]*\) <- \[([^\]]*)\]/ then
        op = $1; relname = $2; vals = $3.split(/; */);
        $last_event = RelEvent.new(op, relname, vals);
        $events.push $last_event;
      when /UPDATE '([^']*)'\[([^\]]*)\]\[([^\]]*)\] := (.*)$/ then
        mapname = $1; ivars = $2; ovars = $3; val = $4;
        ivars = (ivars == "-" ? [] : ivars.split(/; /));
        ovars = (ovars == "-" ? [] : ovars.split(/; /));
        $last_event.add_effect(mapname, ivars, ovars, val);
    end
  end
end

puts "trigger go(id : int) {} = do {"
len = $events.length

$events.each_index do |i|
	if i >= len-1 then puts "  #{$events[i].dispatch_s(true)}";
	else puts "  #{$events[i].dispatch_s(false)}"
  end
end
puts "}"

puts ""
puts "role test {"
puts "  source s1 : int = stream([1])"
puts "  bind s1 -> go"
puts "  consume s1"
puts "}"
puts ""
puts "default role test"
puts ""

def dump_map(mapn)
  map = $maps[mapn]
  sep = "";
  puts "#{mapn} = {"
  $events.each do |evt|
    evt.effects.fetch(mapn, []).each { |u| map.set(*u); }
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
  puts "expected"
  sep = ""
  $maps.each { |mapn,m| puts sep; dump_map(mapn); sep = ", "; }
else
  raise "Partition maps unsupported";
end

#$maps.values.each { |m| puts m.to_s }
#$events.each { |e| puts e.to_s }
