#!/usr/bin/env ruby

# == Synopsis
#
# linkFiles.rb:: Symlink in the requested files to the current directory
#
# == Usage
#
# linkFiles.rb [OPTIONS] ROOTDIR FILE1 [FILE2|...|FILEN]
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
# ROOTDIR
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
scriptName=File.basename($0)

# Specify accepted options
options=GetoptLong.new([ "--help",    "-h", GetoptLong::NO_ARGUMENT],
                       [ "--force",   "-f", GetoptLong::NO_ARGUMENT],
                       [ "--exclude", "-e", GetoptLong::REQUIRED_ARGUMENT],
                       [ "--verbose", "-v", GetoptLong::NO_ARGUMENT])

# Parse the command line options
force=false
verbose=false
excludeDir=""
begin
  options.each do |opt, arg|
    case opt
      when "--help"
        RDoc::usage
        exit 0
      when "--force"
        force=true
      when "--exclude"
        excludeDir=arg
      when "--verbose"
        verbose=true
    end
  end
rescue StandardError=>error_message
  puts("ERROR: #{error_message}")
  puts("Try \"#{scriptName} --help\"\n ")
  exit 1
end

# Check the arguments
if ARGV.length < 2
  puts("\nERROR: Must specify ROOTDIR and at least one FILE.")
  puts("Try \"#{scriptName} --help\"\n ")
  exit 1
end

# Get the arguments
rootDir=ARGV.shift
fileList=ARGV.uniq

# If required, remove already existing files from fileList
saveList=fileList.dup  # For verbose output

fileList.delete_if {|f| File.exist?(f)} unless force

if verbose
  removedFiles=saveList-fileList
  unless removedFiles.empty?
    puts("\n#{scriptName}: Existing files removed from the filelist:")
    removedFiles.each {|f| puts f}
  end
end

# Always remove existing non-symlink files from fileList
saveList=fileList.dup  # For verbose output

fileList.delete_if {|f| File.exist?(f) && !File.symlink?(f)}

if verbose
  removedFiles=saveList-fileList
  unless removedFiles.empty?
    puts("\n#{scriptName}: Existing non-symlink files removed from the filelist:")
    removedFiles.each {|f| puts f}
  end
end

# Search the heirarchy
Find::find(rootDir) do |path|
  p=File.basename(path)           # Just get the filename
  Find.prune if p == excludeDir   # Remove excluded directories from list
  if fileList.include?(p)         # If the current file is in the list...
    FileUtils.ln_sf(path,"./")    #   Link it
    fileList.delete(p)            #   Remove it from the list
  end
end

# Check for file that were not found
if not fileList.empty?
  puts("\n#{scriptName}: Files not found in the #{rootDir} heirarchy:")
  fileList.each {|f| puts(f)}
end

