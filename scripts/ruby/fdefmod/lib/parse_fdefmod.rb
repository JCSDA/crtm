#!/usr/bin/env ruby
#
# == Synopsis
#
# parse_fdefmod.rb:: Create Fortran95 definition module from FDefMod Type file
#
# == Usage
#
# parse_fdefmod.rb --help FILE1 [FILE2|...|FILEN]
#
# --help (-h):
#    you're looking at it
#
# Written by:: Paul van Delst, CIMSS/SSEC 01-Dec-2006 (paul.vandelst@ssec.wisc.edu)
#

require 'getoptlong'
require 'rdoc/usage'
require 'fdefmod'

# Define the script name for messages
scriptName=File.basename($0)

# Specify accepted options
options=GetoptLong.new([ "--help",    "-h", GetoptLong::NO_ARGUMENT])

# Parse the command line options
begin
  options.each do |opt, arg|
    case opt
      when "--help"
        RDoc::usage
        exit 0
    end
  end
rescue StandardError=>error_message
  puts("ERROR: #{error_message}")
  puts("Try \"#{scriptName} --help\"\n ")
  exit 1
end

# Check the arguments
if ARGV.length == 0
  puts("\nNo FDefMod Type files specified, nothing to do. Exiting...")
  exit 0
end

# Process type files
ARGV.each do |typeFile|
  f=FDefMod.new
  moduleCode=f.createModule(typeFile)
  moduleName=f.structName+"_Define.f90"
  open(moduleName,'w') {|f95File| f95File.puts(moduleCode) }
end
