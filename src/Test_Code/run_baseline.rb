#!/usr/bin/env ruby

# == Synopsis
#
# run_baseline.rb::  Run the CRTM baseline tests/verification. The makefiles
#                    do most of the work... this script just glues things
#                    together.
#
# == Usage
#
# run_baseline.rb [OPTION]
#
# If no options are specified, then nothing is done.
# 
# --help  (-h)
#    you're looking at it.
#
# --dry-run  (-n)
#    echo the commands to be executed, but do nothing
#
# --compare (-c)
#    Compare the current output with the baseline files. If not specified,
#    the output is generated but no comparison is performed.
#
# --forward  (-f)
#    Run the forward model tests.
#
# --tangent-linear  (-t)
#    Run the tangent-linear model tests.
#
# --adjoint  (-a)
#    Run the adjoint model tests.
#
# --k-matrix  (-k)
#    Run the k-matrix model tests.
#
# --everything  (-e)
#    Run all the CRTM baseline tests. Overrides any of the individual
#    test run options.
#
# --lahey  [LINUX ONLY]
#    Build the test code using the Lahey Fortran95 compiler.
#
# --pgi  [LINUX ONLY]
#    Build the test code using the Lahey Fortran95 compiler.
#
# --gfortran  [LINUX ONLY]
#    Build the test code using the GNU gfortran Fortran95 compiler.
#    This is the default CRTM compiler for linux.
#
# --g95  [LINUX ONLY]
#    Build the test code using the g95 Fortran95 compiler.
#
#
# CREATION HISTORY:
#       Written by:     Paul van Delst, 14-Nov-2008
#                       paul.vandelst@noaa.gov
#

require 'fileutils'
require 'getoptlong'
require 'rdoc/usage'

# Parameters
SUCCESS=0
FAILURE=1
MAKE_STAGES=["link", "build", "run"]

# Specify directories and defaults
models={:fwd => {:dir => "Forward"       , :run => false},
        :tl  => {:dir => "Tangent_Linear", :run => false},
        :ad  => {:dir => "Adjoint"       , :run => false},
        :k   => {:dir => "K_Matrix"      , :run => false} }
noop = false
compare = "n"
make_flags = ""

# Specify accepted options
options=GetoptLong.new(
  [ "--help",           "-h", GetoptLong::NO_ARGUMENT ],
  [ "--dry-run",        "-n", GetoptLong::NO_ARGUMENT ],
  [ "--compare",        "-c", GetoptLong::NO_ARGUMENT ],
  [ "--forward",        "-f", GetoptLong::NO_ARGUMENT ],
  [ "--tangent-linear", "-t", GetoptLong::NO_ARGUMENT ],
  [ "--adjoint",        "-a", GetoptLong::NO_ARGUMENT ],
  [ "--k-matrix",       "-k", GetoptLong::NO_ARGUMENT ],
  [ "--everything",     "-e", GetoptLong::NO_ARGUMENT ],
  [ "--gfortran",             GetoptLong::NO_ARGUMENT ],
  [ "--pgi",                  GetoptLong::NO_ARGUMENT ],
  [ "--g95",                  GetoptLong::NO_ARGUMENT ],
  [ "--lahey",                GetoptLong::NO_ARGUMENT ] )

# Parse the command line options
begin
  options.each do |opt, arg|
    case opt
      when "--help"
        RDoc::usage
        exit SUCCESS
      when "--dry-run"
        noop = true
      when "--compare"
        compare = "y"
      when "--forward"
        models[:fwd][:run] = true
      when "--tangent-linear"
        models[:tl][:run]  = true
      when "--adjoint"
        models[:ad][:run]  = true
      when "--k-matrix"
        models[:k][:run]   = true
      when "--everything"
        models.each_key {|k| models[k][:run] = true}
      when "--gfortran"
        make_flags = 'TARGET="gfortran"'
      when "--lahey"
        make_flags = 'TARGET="lahey" RUN_OPTS="-Wl,-T"'
      when "--pgi"
        make_flags = 'TARGET="pgi"'
      when "--g95"
        make_flags = 'TARGET="g95"'
    end
  end
rescue StandardError=>error_message
  puts "ERROR: #{error_message}"
  RDoc::usage
  exit FAILURE
end

# If no models are to be tested, say so and display help
if models.values.collect {|v| !v[:run]}.all? then
  puts("\n No model tests selected.")
  RDoc::usage
  exit SUCCESS
end

# Set the make noop switch if necessary
noop_flag = noop ? "-n" : ""

# Create the Test.Default_Input file
File.open("Test.Default_Input",'w') {|f| f.write("#{compare}\n")}

# Remove the test report file
report_file = "Test.Report"
begin
  File.delete(report_file) if File.exists?(report_file)
rescue
  puts("Error removing old test report log file!")
  exit FAILURE
end

# Run each selected model
begin
  models.each_key do |k|
    next if not models[k][:run]
    # Invoke the make file for current model
    make_command = "make #{noop_flag} #{models[k][:dir].downcase} #{make_flags}"
    puts(make_command) if noop
    raise unless system(make_command)
    # Check if signal file was created
    next if noop
    if File.exists?("#{models[k][:dir]}/Test_#{models[k][:dir]}.signal") then
      puts("\n  ALL #{models[k][:dir]} TESTS PASSED!")
    else
      puts("\n  Failed #{models[k][:dir]} test detected. <--------**********")
    end
  end
rescue
  puts("Error running #{models[k][:dir]} model test!")
  exit FAILURE
end
