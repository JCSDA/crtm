#!/usr/bin/env ruby

# == Synopsis
#
# diffBaseline.rb:: Difference the CRTM versioned baseline output with the
#                   output from the various test codes for a specified sensor.
#
# == Usage
#
# diffBaseline.rb [OPTION] sensorId1 [ sensorId2 sensorId3 ...]
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
# sensorId1 [ sensorId2 sensorId3 ...]
#    The string ids for the sensor results to difference. Examples are
#    amsua_n17, hirs3_n17, ssmis_f16, imgr_g11, etc..
#

require 'getoptlong'
require 'rdoc/usage'
 
# Specify defaults
forward      =false
tangentlinear=false
adjoint      =false
kmatrix      =false

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
        forward=true
      when "--tangent-linear"
        tangentlinear=true
      when "--adjoint"
        adjoint=true
      when "--k-matrix"
        kmatrix=true
      when "--everything"
        forward      =true
        tangentlinear=true
        adjoint      =true
        kmatrix      =true
    end
  end
rescue StandardError=>error_message
  puts "ERROR: #{error_message}"
  RDoc::usage
  exit 1
end

# Get the sensorID
if ARGV.empty?
  puts("Missing sensorId argument (try --help)")
  exit 1
end
sensorId=ARGV

# The test directory names
modelDir =["Forward","Tangent_Linear","Adjoint","K_Matrix"]
modelTest=[forward  , tangentlinear  , adjoint , kmatrix  ]

# Test each model
modelDir.each_with_index do |dir, index|
  next if not File.directory?(dir) or not modelTest[index]
  sensorId.each do |id|
    new=dir+"/"+id+".CRTM_Test_"+dir+".output"
    baseline=new+".Baseline"
    system("tkdiff #{baseline} #{new}") if File.file?(baseline) and File.file?(new)
  end
end
