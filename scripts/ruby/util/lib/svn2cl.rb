#!/usr/bin/env ruby

# == Synopsis
#
# Create ChangeLog file from subversion commit log messages.
#
# == Usage
#
# svn2cl.rb [OPTIONS] [ARGUMENTS]
#
# == Options
#
# --help  (-h):
#    you're looking at it
#
#
# == Arguments
#
#
#
# Written by:: Paul van Delst, CIMSS/SSEC 23-Jan-2007 (paul.vandelst@ssec.wisc.edu)
#

require 'getoptlong'
require 'rdoc/usage'

# Specify accepted options
options=GetoptLong.new(
  [ "--help",       "-h", GetoptLong::NO_ARGUMENT ] )

# Parse the command line options
begin
  options.each do |opt, arg|
    case opt
      when "--help"
        RDoc::usage
        exit 0
    end
  end
rescue StandardError=>errormessage
  puts "ERROR: #{error_message}"
  RDoc::usage
  exit 1
end

header = false
hdr  = []
body = []

i = -1
log = ""

`svn log`.each do |line|
  if line =~ /^-+$/
    header = true                    # Next line is a new header
    body[i] = log unless i == -1     # Save previous log message body
    i += 1
    log = ""
    next
  end
  if header
    hdr[i] = line.split("|")[0..-2]  # Save current header
    header=false
    next
  end
  log << line                        # Accumulate current log message body
end

File.open("test.hdr","w") {|f| f.puts(hdr)}
File.open("test.body","w") {|f| f.puts(body)}
