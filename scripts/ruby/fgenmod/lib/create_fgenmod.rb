#!/usr/bin/env ruby
#
# == Synopsis
#
# create_fgenmod.rb:: Create Fortran95 definition and I/O
#                     modules from and FGenMod Type file
#
# == Usage
#
# create_fgenmod.rb --help FILE1 [FILE2|...|FILEN]
#
# --help (-h):
#    you're looking at it
#
# Written by:: Paul van Delst, CIMSS/SSEC 01-Dec-2006 (paul.vandelst@ssec.wisc.edu)
#

require 'getoptlong'
require 'rdoc/usage'
require 'fgenmod'

# Define the script name for messages
script_name=File.basename($0)

# Specify accepted options
options=GetoptLong.new([ "--help", "-h", GetoptLong::NO_ARGUMENT],
                       [ "--debug","-d", GetoptLong::NO_ARGUMENT])

# Set defaults
debug=false

# Parse the command line options
begin
  options.each do |opt, arg|
    case opt
      when "--debug"
        debug=true
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
  puts("\nNo FGenMod Type files specified, nothing to do. Exiting...")
  exit 0
end

# Process type files
ARGV.each do |type_file|
  FGenMod::Generator.new.generate(type_file)
end
