#!/usr/bin/env ruby

# == Synopsis
#
# linkFiles.rb:: Symlink in the requested files to the current directory
#
# == Usage
#
# linkFiles.rb [OPTIONS] root_dir FILE1 [FILE2|...|FILEN]
#
# --help (-h):
#    you're looking at it
#
# --exclude <dir> (-e <dir>):
#    Use this option to exclude directory heirarchies from the search
#    for files to symlinkout.
#
# --force (-f)
#    Use this option to force a symlink to the found file, even if a symlink
#    already exists.
#
# --verbose (-v)
#    Use this option to specify more output as the script does its thing.
#
# root_dir
#    Directory at which to begin the search for the requested files to symlink.
#
# FILE1 [FILE2 | ... | FILEN]
#    List of files for which a symlink if required.
#    
# Written by:: Paul van Delst, CIMSS/SSEC 10-Nov-2006 (paul.vandelst@ssec.wisc.edu)
#

require 'getoptlong'
require 'rdoc/usage'
require 'find'
require 'fileutils'

# Define the script name for messages
script_name = File.basename($0)

# Specify accepted options
options = GetoptLong.new([ "--help",    "-h", GetoptLong::NO_ARGUMENT],
                         [ "--force",   "-f", GetoptLong::NO_ARGUMENT],
                         [ "--exclude", "-e", GetoptLong::REQUIRED_ARGUMENT],
                         [ "--verbose", "-v", GetoptLong::NO_ARGUMENT])

# Parse the command line options
force = false
verbose = false
exclude_dir = []
begin
  options.each do |opt, arg|
    case opt
      when "--help"
        RDoc::usage
        exit 0
      when "--force"
        force = true
      when "--exclude"
        exclude_dir << arg
      when "--verbose"
        verbose = true
    end
  end
rescue StandardError => error_message
  puts("ERROR: #{error_message}")
  puts("Try \"#{script_name} --help\"\n ")
  exit 1
end

# Check the arguments
if ARGV.length < 2
  puts("\nERROR: Must specify root_dir and at least one FILE.")
  puts("Try \"#{script_name} --help\"\n ")
  exit 1
end

# Get the arguments
root_dir = ARGV.shift
file_list = ARGV.uniq

# If required, remove already existing files from file_list
save_list = file_list.dup  # For verbose output

file_list.delete_if {|f| File.exist?(f)} unless force

if verbose
  removed_files = save_list-file_list
  unless removed_files.empty?
    puts("\n#{script_name}: Existing files removed from the file_list:")
    removed_files.each {|f| puts f}
  end
end

# Always remove existing non-symlink files from file_list
save_list = file_list.dup  # For verbose output

file_list.delete_if {|f| File.exist?(f) && !File.symlink?(f)}

if verbose
  removed_files = save_list-file_list
  unless removed_files.empty?
    puts("\n#{script_name}: Existing non-symlink files removed from the file_list:")
    removed_files.each {|f| puts f}
  end
end

# Search the heirarchy
Find::find(root_dir) do |path|
  p=File.basename(path)                          # Just get the filename
  exclude_dir.each {|e| Find.prune if p == e }   # Remove excluded directories from list
  if file_list.include?(p)                       # If the current file is in the list...
    FileUtils.ln_sf(path,"./")                   #   Link it
    file_list.delete(p)                          #   Remove it from the list
  end
end

# Check for file that were not found
if not file_list.empty?
  puts("\n#{script_name}: Files not found in the #{root_dir} heirarchy:")
  file_list.each {|f| puts(f)}
end

