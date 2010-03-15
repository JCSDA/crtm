#!/usr/bin/env ruby

# == Synopsis
#
# collect_coeffs.rb:: Create tarball of the CRTM coefficients.
#
# == Usage
#
# collect_coeffs.rb [OPTIONS]
#
# --help (-h):
#    you're looking at it
#
# --exclude (-e):
#    Excludes the sensor-independent files (e.g. CloudCoeff) from
#    the tarball.
#
#    
# Written by:: Paul van Delst, 02-Feb-2009 (paul.vandelst@noaa.gov)
#

require 'getoptlong'
require 'rdoc/usage'
require 'fileutils'

# Define constants and defaults
FIXFILE_ROOT = ENV['CRTM_FIXFILE_ROOT']
FIXFILE_TAR = "CRTM_Coefficients"
FIXFILE_INFO = [{:name=>"TauCoeff/ODAS", :sensor=> true},
                {:name=>"TauCoeff/ODPS", :sensor=> true},
                {:name=>"SpcCoeff"     , :sensor=> true},
                {:name=>"AerosolCoeff" , :sensor=> false},
                {:name=>"CloudCoeff"   , :sensor=> false},
                {:name=>"EmisCoeff"    , :sensor=> false}]
FIXFILE_FORMAT = ["Big_Endian","Little_Endian"]
exclude = false
     
# Accepted command line options
OPTIONS = GetoptLong.new(
  [ "--help"      , "-h", GetoptLong::NO_ARGUMENT],
  [ "--exclude"   , "-e", GetoptLong::NO_ARGUMENT] )


# Process arguments
begin
  OPTIONS.each do |opt, arg|
    case opt
      when "--help"
        RDoc::usage(0)
      when "--exclude"
        exclude = true
    end
  end
rescue ArgumentError => error_message
  puts("ERROR: #{error_message}")
  puts("Try \"#{File.basename($0)} --help\"\n ")
  exit 1
end


puts("Collecting coefficient files from #{FIXFILE_ROOT}...")
begin
  FileUtils.chdir(FIXFILE_ROOT) do

    # Create the working directory used to generate tarball
    FileUtils.rm_rf(FIXFILE_TAR,:secure=>true) if File.exists?(FIXFILE_TAR)
    FileUtils.mkdir(FIXFILE_TAR)
    
    # Enter the working directory
    FileUtils.chdir(FIXFILE_TAR) do
    
      # Loop over each type of Coeff file
      FIXFILE_INFO.each do |type|
      
        # Skip this type if requested
        next if exclude && !type[:sensor]
        puts("  Linking in #{type[:name]} directories...")
        
        # Create and enter the coefficient directory
        FileUtils.mkdir_p(type[:name])
        FileUtils.chdir(type[:name]) do

          # Link in each type of Coeff format
          FIXFILE_FORMAT.each {|f| FileUtils.ln_s("#{FIXFILE_ROOT}/#{type[:name]}/#{f}","#{f}")}

        end
      end
    end
  
    # Create a tarball of the created directory
    puts("  Creating tarball...")
    system("tar chf #{FIXFILE_TAR}.tar --exclude .svn ./#{FIXFILE_TAR}")
    puts("  Compressing tarball...")
    system("gzip -f #{FIXFILE_TAR}.tar")
    FileUtils.rm_rf("./#{FIXFILE_TAR}")

  end
  
rescue Exception => error_message
  puts "ERROR: #{error_message}"
  exit(1)
end

