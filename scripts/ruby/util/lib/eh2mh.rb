#!/usr/bin/env ruby

# == Synopsis
#
# Replace "Error_Handler" with "Message_Handler" strings in file
#
# == Usage
#
# eh2mh.rb [OPTION] filename
#
# where the available OPTIONs are
#
# -h, --help:
#    you're looking at it
#
# -n, --no-backup:
#    do not create a backup file before modification. 
#    Backup file name is "filename".bak
#

require 'getoptlong'
require 'rdoc/usage'

# Specify accepted options
options=GetoptLong.new(
  [ "--help",      "-h", GetoptLong::NO_ARGUMENT ],
  [ "--no-backup", "-n", GetoptLong::NO_ARGUMENT ] )

# Define backup default
$-i=".bak"

# Parse the command line options
begin
  options.each do |opt, arg|
    case opt
      when "--help"
        RDoc::usage
        exit 0
      when "--no-backup"
        $-i=""
    end
  end
rescue StandardError=>errormessage
  puts "#{error_message} (try --help)"
  exit 1
end

# Get the filename
if ARGV.empty?
  puts "Missing file argument (try --help)"
  exit 1
end

# Inplace edit the file
ARGF.each do |line|
  line.gsub!(/error_handler:  /i, 'Message_Handler:')  # The module header
  line.gsub!(/error_handler/i, 'Message_Handler')      # Everything else
  puts line
end

# Note that one could also do:
#  ruby -pi.bak -e "gsub(/error_handler/i,'Message_Handler')" *.f90
# on the command line.
