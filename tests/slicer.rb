#!/usr/bin/env ruby

require 'getoptlong';
require 'thread';
require 'logger';

$slicer_log = Logger.new(STDOUT);
$slicer_log.level = Logger::DEBUG;
$default_slicer_path = "~/Dropbox/CodeSnippets";
$remote_slicers = Hash.new { |h,k| h[k] = RemoteSlicer.new(k) }

module LoggerMixins
  @@logger_class = ""
  
  def logmsg(msg, throwable)
    @@logger_class + msg + (throwable.nil? ? "" : (" "+throwable.to_s));
  end

  def debug(throwable = nil)
    $slicer_log.debug(logmsg(yield, throwable)) if $slicer_log;
  end

  def info(throwable = nil)
    $slicer_log.info(logmsg(yield, throwable)) if $slicer_log;
  end

  def warn(throwable = nil)
    $slicer_log.warn(logmsg(yield, throwable)) if $slicer_log;
  end

  def error(throwable = nil)
    $slicer_log.error(logmsg(yield, throwable)) if $slicer_log;
  end

  def fatal(throwable = nil)
    $slicer_log.fatal(logmsg(yield, throwable)) if $slicer_log;
  end
  
  
  module LoggerMixinClassMethods
    def logger_class=(logger_class)
      self.instance_variable_set(
        :@logger_class, 
        @@logger_class = "[ "+logger_class + " ]: "
      );
    end
  end
  
  def self.included(klass)
    klass.extend(LoggerMixinClassMethods);
  end
end


class RemoteSlicer
  attr_reader :ready;
  attr_writer :ready;
  
  include LoggerMixins;
  logger_class = "RemoteProcess";
  
  def initialize(host, path = $default_slicer_path)
    cmd = "ruby " + path + "/slicer.rb --run-as-leaf";
    @cmd, @host = cmd, host;
    info { "SSH connecting to #{host}" }
    @pipe = IO.popen("ssh #{@host} '#{cmd}'", "w+");
    @ready = false;
    
    info { "SSH connected to #{host}, starting command #{cmd}" }
    @thread = Thread.new(@pipe) do |ssh|
      info { "SSH pid " + ssh.pid.to_s + " starting: " + cmd.chomp; }
      ssh.each do |line|
        line = line.chomp;
        debug { "READ LINE: #{line}" }
        if @ready then
          receive(line)
        else
          if line == "SLICER READY" then
            @ready = true;
          else
            error { "Expected ready message, but got '#{line}'" }
          end
        end
      end
      info { "SSH pid " + ssh.pid.to_s + " complete"; }
    end
    at_exit do 
      send("QUIT");
      Process.wait(@pipe.pid); 
      info { "Killed SSH to " + @host; }
    end
  end
  
  def receive(msg)
    puts msg;
  end
  
  def send(msg)
     @pipe.puts(msg);
  end
  
  def status
    if @thread.status then true else false end;
  end
end

def leaf_command_loop()
  host = `hostname`.split(".")[0];
  logfile = "/dev/null";
  puts "SLICER READY";
  STDOUT.flush;
  STDIN.each do |line|
    line = line.chomp;
    case line
      when "QUIT" then
        STDIN.close();
        exit(0);
      when /LOG (.*)/ then
        logfile = "#{$1}_#{host}.log";
      when /START (.*)/ then
        pid = Process.fork do
          Process.exec("#{$1} 2>&1 > #{logfile}");
        end
        Process.detach(pid);
        at_exit { Process.kill("HUP", pid); }
      when /EXEC (.*)/ then
        pid = Process.fork do
          Process.exec("#{$1} 2>&1 > #{logfile}");
        end
        at_exit { Process.wait(pid); }
      else 
        puts line;
    end
    STDOUT.flush;
  end
end

def dispatch(arg)
  arg = arg.split(":");
  if arg.length > 1 then
    host = arg[0]; arg = arg[1];
    yield($remote_slicers[arg[0]], arg[1])
  else
    $remote_slicers.each do |host, slicer|
      yield(slicer, arg[0])
    end
  end
end

def process_command(cmd, arg)
  $slicer_log.debug("Command: #{cmd}('#{arg}')");
  case cmd
    when "connect" then 
      $remote_slicers[arg];
    when "log" then 
      dispatch(arg) { |slicer, arg| slicer.send("LOG #{arg}") }
    when "start" then 
      dispatch(arg) { |slicer, arg| slicer.send("START #{arg}") }
    when "exec" then 
      dispatch(arg) { |slicer, arg| slicer.send("EXEC #{arg}") }
    
    else $slicer_log.fatal("Unknown command: #{cmd}");
  end
end

GetoptLong.new(
  [ '--run-as-leaf', GetoptLong::NO_ARGUMENT ],
  [ '--connect', GetoptLong::REQUIRED_ARGUMENT],
  [ '--start', GetoptLong::REQUIRED_ARGUMENT],
  [ '--exec', GetoptLong::REQUIRED_ARGUMENT],
  [ '--log', GetoptLong::REQUIRED_ARGUMENT]
).each do |opt, arg|
  if opt == '--run-as-leaf' then
    leaf_command_loop()
  else
    process_command(opt[2..-1], arg)
  end
end

ARGV.each do |arg|
  File.open(arg) do |f|
    f.readlines.each do |line|
      if /([^# ][^ ]*) (.*)/ =~ line
        then process_command($1, $2)
      end
    end
  end
end