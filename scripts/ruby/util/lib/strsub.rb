#!/usr/bin/env ruby

# == Synopsis
#
# Substitute strings in files
#
# == Usage
#
# strsub.rb [OPTION] old new file1 [file2 file3 file4 ...]
#
# == Options
#
# --help  (-h):
#    you're looking at it
#
# --ignore-case  (-i)
#    ignore the case of the string to replace
#
# --no-backup  (-n):
#    do not create a backup file before modification. 
#    Backup file name is "filename".bak
#
# == Arguments
#
# old:
#    string to replace
#
# new:
#    replacement string
#
# file1 [file2 file3 file4 ...]:
#    files on which to perform the substitution
#
#
# Written by:: Paul van Delst, CIMSS/SSEC 11-Aug-2006 (paul.vandelst@ssec.wisc.edu)
#

require 'getoptlong'
require 'rdoc/usage'

# Specify accepted options
options=GetoptLong.new(
  [ "--help",       "-h", GetoptLong::NO_ARGUMENT ],
  [ "--ignore-case","-i", GetoptLong::NO_ARGUMENT ],
  [ "--no-backup",  "-n", GetoptLong::NO_ARGUMENT ] )

# Define backup default
$-i=".bak"

# Define defaults
ignorecase=0

# Parse the command line options
begin
  options.each do |opt, arg|
    case opt
      when "--help"
        RDoc::usage
        exit 0
      when "--ignore-case"
        ignorecase=Regexp::IGNORECASE
      when "--no-backup"
        $-i=""
    end
  end
rescue StandardError=>errormessage
  puts "ERROR: #{error_message}"
  RDoc::usage
  exit 1
end

# Check for args
if ARGV.empty?
  puts "ERROR: No arguments supplied"
  RDoc::usage
  exit 1
end

# Get the strings
old=ARGV.shift
new=ARGV.shift

# Build the regex
re=Regexp.new(/#{old}/,ignorecase)

# Inplace edit the file
ARGF.each do |line|
  line.gsub!(re, new)
  puts line
end

# Note that one could also do:
#  ruby -pi.bak -e "gsub(/error_handler/i,'Message_Handler')" *.f90
# on the command line.
