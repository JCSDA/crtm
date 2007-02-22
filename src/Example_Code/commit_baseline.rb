#!/usr/bin/env ruby

# == Synopsis
#
# commit_baseline.rb:: Commit the CRTM baseline output in the repository for the
#                      specified tests and sensors.
#
# == Usage
#
# commit_baseline.rb [OPTION] log_message sensor_id1 [ sensor_id2 sensor_id3 ...]
#
# If no options are specified, then no CRTM model results (forward, tangent-linear,
# adjoint, and K-matrix) are committed.
#
# --help  (-h)
#    you're looking at it.
#
# --dry-run  (-n)
#    echo the commands to be executed, but do nothing
#
# --forward  (-f)
#    Commit the forward model results
#
# --tangent-linear  (-t)
#    Commit the tangent-linear model results
#
# --adjoint  (-a)
#    Commit the adjoint model results
#
# --k-matrix  (-k)
#    Commit the K-matrix model results
#
# --everything  (-e)
#    Commit every set of CRTM model results.
#
# log_message
#    The log message to use when committing the updates to the repository.
#
# sensor_id1 [ sensor_id2 sensor_id3 ...]
#    The string ids for the sensor results to copy. Examples are
#    amsua_n17, hirs3_n17, ssmis_f16, imgr_g11, etc..
#
#
# Written by:: Paul van Delst, CIMSS/SSEC 31-Oct-2006 (paul.vandelst@ssec.wisc.edu)
#

require 'tempfile'
require 'getoptlong'
require 'rdoc/usage'
 
# Specify directories and defaults
models={:fwd => {:dir => "Forward"       , :test => false},
        :tl  => {:dir => "Tangent_Linear", :test => false},
        :ad  => {:dir => "Adjoint"       , :test => false},
        :k   => {:dir => "K_Matrix"      , :test => false} }
noop = false

# Specify accepted options
options=GetoptLong.new(
  [ "--help",           "-h", GetoptLong::NO_ARGUMENT ],
  [ "--dry-run",        "-n", GetoptLong::NO_ARGUMENT ],
  [ "--forward",        "-f", GetoptLong::NO_ARGUMENT ],
  [ "--tangent-linear", "-t", GetoptLong::NO_ARGUMENT ],
  [ "--adjoint",        "-a", GetoptLong::NO_ARGUMENT ],
  [ "--k-matrix",       "-k", GetoptLong::NO_ARGUMENT ],
  [ "--everything",     "-e", GetoptLong::NO_ARGUMENT ] )

# Parse the command line options
begin
  options.each do |opt, arg|
    case opt
      when "--help"
        RDoc::usage
        exit 0
      when "--dry-run"
        noop = true
      when "--forward"
        models[:fwd][:test] = true
      when "--tangent-linear"
        models[:tl][:test]  = true
      when "--adjoint"
        models[:ad][:test]  = true
      when "--k-matrix"
        models[:k][:test]   = true
      when "--everything"
        models.each_key {|k| models[k][:test] = true}
    end
  end
rescue StandardError=>error_message
  puts "ERROR: #{error_message}"
  RDoc::usage
  exit 1
end

# Get the arguments and check
if ARGV.length < 2
  puts("Missing arguments (try --help)")
  exit 1
end
log_message=ARGV.shift
sensor_id=ARGV.uniq

# Build a list of files to commit
file_list = []
models.each_key do |k|
  next if not models[k][:test]
  sensor_id.each do |id|
    file = "#{models[k][:dir]}/#{id}.CRTM_Test_#{models[k][:dir]}.output.Baseline"
    file_list << file if (`svn status #{file}`.length > 0)
  end
end
if file_list.empty?
  puts("No files to commit. Exiting...")
  exit
end

# Create the temporary log message file
log_file = Tempfile.new("commit.log")
log_file << "  * #{file_list.first}\n"
file_list[1..-2].each {|file| log_file << "    #{file}\n"}
log_file << "    #{file_list.last}:\n"
log_file << "    - #{log_message}"
log_file.close

# Check everything in 
cmd="svn commit -F #{log_file.path} #{file_list.join(" ")}"
unless noop: system(cmd)
else         puts(cmd)
end
