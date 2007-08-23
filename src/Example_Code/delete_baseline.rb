#!/usr/bin/env ruby

# == Synopsis
#
# delete_baseline.rb:: Delete current CRTM test output for a specified model
#
# == Usage
#
# delete_baseline.rb [OPTION]
#
# If no options are specified, then no CRTM model results (forward, tangent-linear,
# adjoint, and K-matrix) are deleted.
# 
# --help  (-h)
#    you're looking at it.
#
# --dry-run  (-n)
#    echo the commands to be executed, but do nothing
#
# --forward  (-f)
#    Delete the forward model results
#
# --tangent-linear  (-t)
#    Delete the tangent-linear model results
#
# --adjoint  (-a)
#    Delete the adjoint model results
#
# --k-matrix  (-k)
#    Delete the K-matrix model results
#
# --everything  (-e)
#    Delete every set of CRTM model results.
#
# --svn (-s)
#    Delete the files from the repository also (must still commit)
#

require 'getoptlong'
require 'rdoc/usage'
 
# Specify directories and defaults
models={:fwd => {:dir => "Forward"       , :test => false},
        :tl  => {:dir => "Tangent_Linear", :test => false},
        :ad  => {:dir => "Adjoint"       , :test => false},
        :k   => {:dir => "K_Matrix"      , :test => false} }
noop = false
svn  = false
path = "Results"
        
# Specify accepted options
options=GetoptLong.new(
  [ "--help",           "-h", GetoptLong::NO_ARGUMENT ],
  [ "--dry-run",        "-n", GetoptLong::NO_ARGUMENT ],
  [ "--forward",        "-f", GetoptLong::NO_ARGUMENT ],
  [ "--tangent-linear", "-t", GetoptLong::NO_ARGUMENT ],
  [ "--adjoint",        "-a", GetoptLong::NO_ARGUMENT ],
  [ "--k-matrix",       "-k", GetoptLong::NO_ARGUMENT ],
  [ "--everything",     "-e", GetoptLong::NO_ARGUMENT ],
  [ "--svn",            "-s", GetoptLong::NO_ARGUMENT ] )

# Parse the command line options
begin
  options.each do |opt, arg|
    case opt
      when "--help"
        RDoc::usage
        exit 0
      when "--dry-run"
        noop = true
      when "--svn"
        svn = true
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

# If svn flag set, Baseline flags to be deleted.
ext = svn ? ".Baseline" : ""

# Test each model
models.each_key do |k|
  next if not models[k][:test]
  Dir.glob("#{models[k][:dir]}/#{path}/*.e*.c*.a*.ac*.bin#{ext}").each do |f|
    cmd = "rm -f #{f}"
    cmd << "; svn delete #{f}" if svn
    if noop
      puts cmd
    else
      system(cmd) if File.file?(f)
    end
  end
end
