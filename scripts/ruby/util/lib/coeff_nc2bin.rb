#!/usr/bin/env ruby
# == Synopsis
#
# coeff_nc2bin.rb::  Script to convert CRTM SpcCoeff and TauCoeff coefficient
#                    files from netCDF to the little- and big-endian format
#                    used operationally.
#
# == Usage
#
# coeff_nc2bin.rb [OPTIONS]  SpcCoeff | TauCoeff
#
#
# --help  (-h):
#    you're looking at it.
#
# --file <file>  (-f <file>)
#    Specify the SensorInfo file used to select the sensors for which the
#    coefficient files are to be converted. If not specified, an attempt is
#    made to read a file named "SensorInfo.convert"
#
# --dir <dir>  (-d <dir>)
#    Specify the relative parent directory in which the netCDF, Little_Endian,
#    and Big_Endian subdirectories reside. If not specified, the current
#    directory is used.
#
# --debug (-g):
#    Set this switch to output debug information. Setting this displays the commands
#    to be executed without invoking them.
#
#
# Written by:: Paul van Delst, 29-Apr-2009 (paul.vandelst@noaa.gov)
#

require 'getoptlong'
require 'rdoc/usage'
require 'fileutils'
require 'sensorinfo'

# Defaults
DEFAULT_SENSORINFO = "SensorInfo.convert"
CONVERT_TYPE = [{:name=>"Big_Endian"   , :switch=>"-b"},
                {:name=>"Little_Endian", :switch=>"-l"}]

# Process options
options = GetoptLong.new(
  [ "--help" , "-h", GetoptLong::NO_ARGUMENT       ],
  [ "--debug", "-g", GetoptLong::NO_ARGUMENT       ],
  [ "--file" , "-f", GetoptLong::REQUIRED_ARGUMENT ],
  [ "--dir"  , "-d", GetoptLong::REQUIRED_ARGUMENT ] )

debug = false
file  = DEFAULT_SENSORINFO
dir   = File.dirname("./")
begin
  options.each do |opt, arg|
    case opt
      when "--help"
        RDoc::usage
        exit(0)
      when "--debug"
        debug = true
      when "--file"
        file = arg
        raise "File #{file} not found" if ! File.exists?(file)
      when "--dir"
        dir  = arg
        raise "Directory #{dir} not found" if ! File.directory?(dir)
    end
  end
  raise "Must specify a coefficient file type" if ARGV.empty?
  type = ARGV[0]
rescue RuntimeError => error_message
  puts("\nERROR: #{error_message}")
  puts("Try \"#{File.basename($0)} --help\"\n ")
  exit(1)
end


puts("---> Converting #{type} files...")

begin
  # Read the SensorInfo file
  sensorinfo = SensorInfo::Node.load(file)
  
  # Loop over each output type
  CONVERT_TYPE.each do |endian_type|
    FileUtils.chdir("#{dir}/#{endian_type[:name]}", :verbose=>debug) do
      sensorinfo.each do |s|
        coeff_hdr = "#{s.first}.#{type}"
        nc_file   = "#{coeff_hdr}.nc"
        bin_file  = "#{coeff_hdr}.bin"
        FileUtils.ln_sf("../netCDF/#{nc_file}", nc_file, :verbose=>debug)
        system("run_#{type}_NC2BIN #{endian_type[:switch]}")
        FileUtils.rm(nc_file, :verbose=>debug)
      end
      system("pwd")
    end
  end

rescue Exception => error_message
  puts "ERROR: #{error_message}"
  exit(1)
end


