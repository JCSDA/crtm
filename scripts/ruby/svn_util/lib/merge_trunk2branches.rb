#!/usr/bin/env ruby
#
# == Synopsis
#
# merge_trunk2branches.rb::  Script to merge the CRTM repository trunk to all
#                            the development branches (EXP-* directories) for
#                            which a working copy exists.
#
# == Usage
#
# merge_trunk2branches.rb [OPTIONS] trunk_revision [branch1 branch2 ... branchN]
#
#
# == Options
#
# --help  (-h):
#    you're looking at it.
#
# --noop (-n):
#    Set this switch to display the commands to be executed without invoking them.
#
#
# == Arguments
#
# trunk_revision:
#    The trunk revision identifying the merge END point.
#
# [branch1 branch2 ... branchN]:
#    The branch directories to process. If not specified, all branch directories
#    with the prefix "EXP-" are processed.
#
# Written by:: Paul van Delst, 16-Nov-2009 (paul.vandelst@noaa.gov)
#


# Generic YAML accessors
def read_yaml(file)
  File.open(file) {|f| YAML.load(f)}
end

def write_yaml(file,data)
  File.open(file,"w") {|f| YAML.dump(data,f)}
end

# Mergeinfo file accessors
def read_mergeinfo()
  read_yaml(MERGEINFO_FILE)
end

def write_mergeinfo(mergeinfo)
  write_yaml(MERGEINFO_FILE,mergeinfo)
end

# Conflicts file accessors
def read_conflicts()
  read_yaml(CONFLICTS_FILE)
end

def write_conflicts(conflicts)
  write_yaml(CONFLICTS_FILE,conflicts)
end

# Utility functions
def unresolved_conflicts?
  File.exists?(CONFLICTS_FILE)
end

def update_mergeinfo(mergeinfo,trunk_revision)
  mergeinfo[:datestamp]      = Time.now
  mergeinfo[:begin_revision] = mergeinfo[:end_revision] + 1
  mergeinfo[:end_revision]   = trunk_revision
end

def display_mergeinfo(mergeinfo)
  puts("\n  Mergeinfo values:")
  puts(mergeinfo.to_yaml)
end

def in_conflict?(mergeinfo)
  mergeinfo[:conflicts].length != 0
end

def valid_revision?(mergeinfo,trunk_revision)
  mergeinfo[:end_revision] < trunk_revision
end

def update(mergeinfo,noop=true)
  type = noop ? "status --show-updates --quiet" : "update"
  puts("\n  Performing the branch update...\n")
  cmd = "svn #{type}"
  output = `#{cmd}`.chomp!.split("\n")
  if $? == 0
    output
  else
    false
  end
end

def merge(url,mergeinfo,noop=true)
  dry_run = noop ? "--dry-run " : ""
  puts("\n  Performing the #{dry_run}merge...\n")
  cmd = "svn merge #{dry_run} -r#{mergeinfo[:begin_revision]}:#{mergeinfo[:end_revision]} #{url} ."
  output = `#{cmd}`.chomp!.split("\n")
  if $? == 0
    output
  else
    false
  end
end

def commit(mergeinfo,noop=true)
  commit_message = "#{mergeinfo[:branch]} branch. Merged trunk r#{mergeinfo[:begin_revision]}:#{mergeinfo[:end_revision]} into the #{mergeinfo[:branch]} branch."
  puts("\n  Performing the commit...\n")
  cmd = "svn commit -m \"#{commit_message}\""
  output = `#{cmd}`.chomp!.split("\n") unless noop
  if $? == 0
    output
  else
    false
  end
end

def extract_conflicts(merge_output)
  conflicts = []
  merge_output.each {|m| conflicts << m if m =~ %r{^C}}
  conflicts
end

def display_conflicts(conflicts)
  puts("\n  Conflicts found in merge:")
  conflicts.each {|c| puts("#{c}")}
end



# Start the main script
# #####################

require 'getoptlong'
require 'rdoc/usage'
require 'yaml'


# Definitions
# ...Constants
TRUNK_URL = "https://svnemc.ncep.noaa.gov/projects/crtm/trunk"
BRANCHES = Dir["EXP-*"]
MERGEINFO_FILE=".mergeinfo"
CONFLICTS_FILE=".conflicts"
LOG_FILE="log_file"
# ...Defaults
noop = false


# Accepted command line options
OPTIONS = GetoptLong.new(
  [ "--help", "-h", GetoptLong::NO_ARGUMENT],
  [ "--noop", "-n", GetoptLong::NO_ARGUMENT] )


# Process the command line options
begin
  OPTIONS.each do |opt, arg|
    case opt
      when "--help"
        RDoc::usage(0)
      when "--noop"
        noop = true
      else
        raise GetoptLong::InvalidOption, "Invalid option #{opt}"
    end
  end
rescue GetoptLong::InvalidOption => error_message
  puts("\n  ERROR: #{error_message}")
  puts("  Try \"#{File.basename($0)} --help\"\n ")
  exit(1)
end

# Process the command line arguments
trunk_revision = 0
begin
  raise ArgumentError, "Must specify a trunk revision end point" if ARGV.empty?
  trunk_revision = ARGV[0].to_i
  branches = ARGV[1..-1]
rescue Exception => error_message
  puts("\n  ERROR: #{error_message}")
  puts("  Try \"#{File.basename($0)} --help\"\n ")
  exit(1)
end
branches = branches.empty? ? BRANCHES : branches


# Begin branch loop
cwd = Dir.pwd
branches.each do |branch|

  next unless File.exists?("#{branch}/#{MERGEINFO_FILE}")
  
  begin
    Dir.chdir(branch) do

      puts("\n\n*************************")
      
      # Check if there are any unresolved conflicts
      if unresolved_conflicts?
        puts("\n  In directory #{branch}. Working copy in apparent conflict from previous merge. Skipping...")
        break
      end

      
      # Read the merge info file
      mergeinfo = read_mergeinfo()
      puts("Working on #{branch} branch...\n\nBegin time: #{Time.now}\n")
      # ...Temporarily add the branch name to mergeinfo
      mergeinfo[:branch] = branch


      # Update the branch
      update_output = update(mergeinfo,noop)
      raise "Update failed!" unless update_output
      # ...Extract any conflicts
      conflicts = extract_conflicts(update_output)
      # ...Display and go to next branch if conflicts found
      if conflicts.length > 0
        display_conflicts(conflicts)
        write_conflicts(conflicts) unless noop
        break
      end
      

      # Check that trunk revision is greater than previous end_revision
      raise "Trunk revision not valid for merge!" if !valid_revision?(mergeinfo,trunk_revision)


      # Update mergeinfo for current merge
      update_mergeinfo(mergeinfo,trunk_revision)
      display_mergeinfo(mergeinfo)
      write_mergeinfo(mergeinfo) unless noop


      # Perform the merge in the local working copy
      merge_output = merge(TRUNK_URL,mergeinfo,noop)
      raise "Merge failed!" unless merge_output
      # ...Extract any conflicts
      conflicts = extract_conflicts(merge_output)
      # ...Display and go to next branch if conflicts found
      if conflicts.length > 0
        display_conflicts(conflicts)
        write_conflicts(conflicts) unless noop
        break
      end
    

      # Commit the merge
      unless noop
        commit_output = commit(mergeinfo,noop)
        raise "Commit failed!" unless commit_output
      end
      puts("End time: #{Time.now}\n")

    end
    
  rescue => error_message
    puts("\n  ERROR: #{error_message}")
    break
  end

end

