#!/usr/bin/env ruby


# == Synopsis
#
# create_release_tag.rb:: Script to create a release of the CRTM from a tag working copy.
#
# == Usage
#
# create_release_tag.rb [OPTIONS] TAG_WCPATH RELEASE_WCPATH
#
# --help  (-h)
#    you're looking at it.
#
# --no-commit  (-n)
#    Create the release working copy, but do not commit to the repository.
#
# TAG_WCPATH
#    The path to an existing working copy of tagged version of the CRTM.
#
# RELEASE_WCPATH
#    The path in which the release is to be created.
#
#
# Written by:: Paul van Delst, 10-Aug-2011 (paul.vandelst@noaa.gov)
#

require 'optparse'
require 'fileutils'
require 'svn'
require 'inventory'

# Specify parameters
CRTM_ENV = [{:name => "CRTM_SOURCE_ROOT"    ,:dir => "/src"},
            {:name => "CRTM_FIXFILE_ROOT"   ,:dir => "/fix"},
            {:name => "CRTM_TEST_ROOT"      ,:dir => "/test"},
            {:name => "CRTM_EXTERNALS_ROOT" ,:dir => "/externals"},
            {:name => "CRTM_SCRIPTS_ROOT"   ,:dir => "/scripts"},
            {:name => "CRTM_DOC_ROOT"       ,:dir => "/doc"},
            {:name => "CRTM_VALIDATION_ROOT",:dir => "/validation"}]
INVENTORY_FILE = "inventory.txt"


# Helper procedures
def set_crtm_environment
  ENV["CRTM_ROOT"] = FileUtils.pwd()
  CRTM_ENV.each do |env|
    ENV[env[:name]] = ENV["CRTM_ROOT"]+env[:dir]
  end
end


# Command line option processing
options = {}
# ...Specify defaults
options[:commit] = true
# ...Specify the options
opts = OptionParser.new do |opts|
  opts.banner = "Usage: create_release_tag.rb [options] tag_wcpath release_wcpath"

  opts.on("-n", "--no-commit", "create release, but do not commit to the repository") do |n|
    options[:commit] = n
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


# Begin error handling
begin
  # ...Check the arguments
  raise "Must specify existing TAG working copy directory, and RELEASE directory to create." if ARGV.length < 2
  tag_wcpath = ARGV[0]
  release_wcpath = ARGV[1]

  svn = Svn_Util::Svn.new()
  puts svn.versioned?(release_wcpath)

# TEMPORARY
if File.directory?(release_wcpath)
  system("svn revert --depth infinity #{release_wcpath}")
  FileUtils.rm_rf(release_wcpath)
end
# TEMPORARY


  raise "#{release_wcpath} already exists!" if File.directory?(release_wcpath)
  raise "Error creating #{release_wcpath}" unless svn.mkdir(release_wcpath)

  dest = FileUtils.pwd()+"/#{release_wcpath}"
  
  FileUtils.cd(tag_wcpath) do
    raise "#{tag_wcpath} is not a Subversion working copy!" unless File.directory?(".svn")
    raise "#{tag_wcpath} is not a CRTM root directory!" unless File.exist?("Set_CRTM_Environment.sh")
    set_crtm_environment()
    FileUtils.cd("src") do
      system("make create_links")
      FileUtils.cd("Build") do
        inventory = Svn_Util::Inventory.new(INVENTORY_FILE)
        inventory.files.each {|f| svn.copy(f,"#{dest}/#{f}",:parents => true)}
        inventory.linkto.each {|k,v| svn.copy(v,"#{dest}/#{k}",:parents => true)}
      end
    end
  end
  
  svn.commit(release_wcpath,:message => "Committing release #{release_wcpath}") if options[:commit]


# Inform user why script barfed
rescue Exception => error_message
  puts("\nERROR: #{error_message}\n")
  exit(1)
end   
