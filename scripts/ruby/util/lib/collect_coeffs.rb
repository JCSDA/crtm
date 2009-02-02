#!/usr/bin/env ruby

# == Synopsis
#
# collect_Coeff.rb:: Extract out the CRTM coefficients for the specified
#                    instruments. Non-instrument specific coefficient files
#                    are also pulled out.
#
# == Usage
#
# collect_Coeff.rb [-h] [-l] id1 [id2 id3 ....]
#
# -h, --help:
#    you're looking at it
#
# -l, --little:
#    Extract the little-endian format files. By default, big-endian files
#    are extracted.
#
# id1 [id2 id3 ....]
#    Instrument sensor ID strings following the format <sensor>_<platform>.
#    Examples are hirs3_n15, mhs_n18, etc. An error is raised if no matching
#    sensors are found.
#    
# Written by:: Paul van Delst, CIMSS/SSEC 11-Aug-2006 (paul.vandelst@ssec.wisc.edu)
#

require 'getoptlong'
require 'rdoc/usage'
require 'fileutils'

# Specify accepted options
options=GetoptLong.new(
  [ "--help",     "-h", GetoptLong::NO_ARGUMENT ],
  [ "--little",   "-l", GetoptLong::NO_ARGUMENT ] )

# Defaults
etag="Big_Endian"

# Parse the command line options
begin
  options.each do |opt, arg|
    case opt
      when "--help"
        RDoc::usage
        exit 0
      when "--little"
        etag="Little_Endian"
    end
  end
rescue StandardError=>error_message
  puts "ERROR: #{error_message}"
  RDoc::usage
  exit 1
end

if ARGV.length == 0
  puts "\nERROR: Missing sensor id argument(s)"
  RDoc::usage
  exit 1
end

puts("---> Collecting #{etag.upcase.split("_").join(" ")} coefficient files...")

begin
  tar="CRTM_Coefficients"
  FileUtils.mkdir(tar)
  ["CloudCoeff","AerosolCoeff","EmisCoeff"].each { |fid| FileUtils.cp("#{fid}/#{etag}/#{fid}.bin","./#{tar}") }
  ARGV.each do |sid|
    ["TauCoeff","SpcCoeff"].each { |fid| FileUtils.cp("#{fid}/#{etag}/#{sid}.#{fid}.bin", "./#{tar}") }
  end
  system("tar cvf #{tar}.tar ./#{tar}")
  system("gzip -f #{tar}.tar")
  FileUtils.remove_dir("./#{tar}",force=true)
rescue StandardError=>error_message
  puts "ERROR: #{error_message}"
  exit 1
end

