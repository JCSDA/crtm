#!/usr/bin/env ruby


# == Synopsis
#
# create_utillib_release.rb:: Script to create a release of a CRTM utility library.
#
# == Usage
#
# create_utillib_release.rb [OPTIONS] LIBRARY_NAME RELEASE_NAME
#
# --help  (-h)
#    you're looking at it.
#
# --no-commit  (-n)
#    Create the release working copy, but do not commit to the repository.
#
# --noop  (-x)
#    Output the commands to be used, but do nothing.
#
# LIBRARY_NAME
#    The name of the library for which a release is to be created.
#
# RELEASE_NAME
#    The name of the release to create, e.g. "v1.0.0".
#
#
# Written by:: Paul van Delst, 08-Jan-2014 (paul.vandelst@noaa.gov)
# $Id$
#

require 'optparse'
require 'fileutils'
require 'svn'
require 'inventory'


# Specify parameters
# ...Inventory file for the file transfer
INVENTORY_FILE = "inventory.txt"


# Command line option processing
options = {}
# ...Specify the options
opts = OptionParser.new do |opts|
  opts.banner = "Usage: create_utillib_release_tag.rb [options] library_name release_name\n$Revision$"

  options[:commit] = true
  opts.on("-n", "--no-commit", "Create release, but do not commit to the repository") do
    options[:commit] = false
  end

  options[:noop] = false
  opts.on("-x", "--noop", "Output the commands to be used, but do nothing") do
    options[:noop] = true
  end

  opts.on_tail("-h", "--help", "Show this message") do
    puts opts
    exit(0)
  end
end
# ...Parse the options
begin
  opts.parse! ARGV
rescue OptionParser::ParseError => error_message
  puts("ERROR --> #{error_message}")
  puts(opts)
  exit(1)
end


# Create a new Svn object
svn = Svn_Util::Svn.new(noop = options[:noop])


# Begin error handling
begin
  # Check the arguments
  raise "Must specify the LIBRARY_NAME and the RELEASE_NAME." if ARGV.length < 2
  library_name = ARGV[0]
  release_name = ARGV[1]

  # Specify the working copy paths
  src_wcpath     = FileUtils.pwd()
  utility_wcpath = ENV["CRTM_HOME"]+"/releases/utility"
  library_wcpath = utility_wcpath+"/"+library_name
  release_wcpath = library_wcpath+"/"+release_name
  # ...Check we are in a working copy
  raise "Current directory is not a Subversion working copy!" unless svn.versioned?(src_wcpath)
  # ...Check that directory name conventions have been followed
  raise 'No "build" directory in current location!' unless File.directory?("build")
  raise "No #{library_name} directory in #{utility_wcpath}" unless File.directory?(library_wcpath)

  # Create the release working copy
  raise "#{release_wcpath} already exists!" if File.directory?(release_wcpath)
  raise "Error creating #{release_wcpath}" unless svn.mkdir(release_wcpath)

  # Specify the destination directory for the subversion commands
  dest = release_wcpath

  # Move over the source tree to the release directory  
  system("make create_links")
  FileUtils.cd("build") do
    inventory = Svn_Util::Inventory.new(INVENTORY_FILE)
    inventory.files.each {|f| svn.copy(f,"#{release_wcpath}/#{f}",:parents => true)}
    inventory.linkto.each {|k,v| svn.copy(v,"#{release_wcpath}/#{k}",:parents => true)}
  end
  
  # Commit the release
  svn.commit(release_wcpath,:message => "#{library_name} #{release_name} release") if options[:commit]


# Inform user why script barfed
rescue Exception => error_message
  puts("\nERROR: #{error_message}\n")
  exit(1)
end   
