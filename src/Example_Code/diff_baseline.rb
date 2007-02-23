#!/usr/bin/env ruby

# == Synopsis
#
# diff_baseline.rb:: Difference the CRTM versioned baseline output with the
#                    output from the various test codes for a specified sensor.
#
# == Usage
#
# diff_baseline.rb [OPTION] sensor_id1 [ sensor_id2 sensor_id3 ...]
#
# If no options are specified, then no CRTM model results (forward, tangent-linear,
# adjoint, and K-matrix) are differenced.
#
# --help  (-h)
#    you're looking at it.
#
# --forward  (-f)
#    Difference the forward model results
#
# --tangent-linear  (-t)
#    Difference the tangent-linear model results
#
# --adjoint  (-a)
#    Difference the adjoint model results
#
# --k-matrix  (-k)
#    Difference the K-matrix model results
#
# --everything  (-e)
#    Difference every set of CRTM model results.
#
# sensor_id1 [ sensor_id2 sensor_id3 ...]
#    The string ids for the sensor results to difference. Examples are
#    amsua_n17, hirs3_n17, ssmis_f16, imgr_g11, etc..
#
#
# Written by:: Paul van Delst, CIMSS/SSEC 31-Oct-2006 (paul.vandelst@ssec.wisc.edu)
#

require 'getoptlong'
require 'rdoc/usage'
 
# Specify directories and defaults
models={:fwd => {:dir => "Forward"       , :test => false},
        :tl  => {:dir => "Tangent_Linear", :test => false},
        :ad  => {:dir => "Adjoint"       , :test => false},
        :k   => {:dir => "K_Matrix"      , :test => false} }

# Specify accepted options
options=GetoptLong.new(
  [ "--help",           "-h", GetoptLong::NO_ARGUMENT ],
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
sensor_id = ARGV.uniq

# Difference each model
models.each_key do |k|
  next if not models[k][:test]
  sensor_id.each do |id|
    new = "#{dir}/${id}.CRTM_Test_#{dir}.dump"
    baseline = "#{new}.Baseline"
    system("tkdiff #{baseline} #{new}") if File.file?(baseline) and File.file?(new)
  end
end

