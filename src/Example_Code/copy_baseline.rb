#!/usr/bin/env ruby

# == Synopsis
#
# copy_baseline.rb:: Copy current CRTM test code output for a specified model and
#                    sensor to the baseline file for respository checkin.
#
# == Usage
#
# copy_baseline.rb [OPTION] sensor_id1 [ sensor_id2 sensor_id3 ...]
#
# If no options are specified, then no CRTM model results (forward, tangent-linear,
# adjoint, and K-matrix) are copied.
# 
# --help  (-h)
#    you're looking at it.
#
# --dry-run  (-n)
#    echo the commands to be executed, but do nothing
#
# --forward  (-f)
#    Copy the forward model results
#
# --tangent-linear  (-t)
#    Copy the tangent-linear model results
#
# --adjoint  (-a)
#    Copy the adjoint model results
#
# --k-matrix  (-k)
#    Copy the K-matrix model results
#
# --everything  (-e)
#    Copy every set of CRTM model results.
#
# sensor_id1 [ sensor_id2 sensor_id3 ...]
#    The string ids for the sensor results to copy. Examples are
#    amsua_n17, hirs3_n17, ssmis_f16, imgr_g11, etc..
#

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

# Get the sensor_id
if ARGV.empty?
  puts("Missing sensor_id argument (try --help)")
  exit 1
end
sensor_id = ARGV

# Test each model
models.each_key do |k|
  next if not models[k][:test]
  sensor_id.each do |id|
    new = "#{models[k][:dir]}/#{id}.CRTM_Test_#{models[k][:dir]}.dump"
    old = "#{new}.Baseline"
    cmd = "cp #{new} #{old}"
    if noop
      puts cmd
    else
      system(cmd) if File.file?(new)
    end
  end
end
