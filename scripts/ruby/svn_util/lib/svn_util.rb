#!/usr/bin/env ruby

# == Synopsis
#
# svn_util.rb:: Script to update a working copy heirarchy from an unversioned
#               heirarchy.
#
# == Usage
#
# svn_util.rb [OPTIONS] SOURCE_DIR URL
#
# --help  (-h)
#    you're looking at it.
#
# --no-commit  (-n)
#    Update the working copy heirarchy, schedule file additions and
#    deletions, but do not commit to the repository.
#
# --file-list filename  (-f filename)
#    Specify a filename for the file inventory. If not specified, the
#    default name is "inventory.txt"
#
# --tag tagname  (-t tagname)
#    Specify a tag name used in the subversion commit. If not specified, the
#    default tag is the current date formatted as "REL.yyyy-mm-dd".
#
# SOURCE_DIR
#    The root of the unversioned directory heirarchy from which the
#    working copy is to be updated.
#
# URL
#    The URL of the Subversion repository heirarchy that will be
#    modifed.
#
#
# Written by:: Paul van Delst, 29-Aug-2007 (paul.vandelst@noaa.gov)
#

require 'getoptlong'
require 'rdoc/usage'

require 'inventory'
require 'svn'

# Specify defaults
commit = true
file   = 'inventory.txt'
tag    = Time.now.strftime("REL.%Y-%m-%d")
        
# Specify accepted options
options=GetoptLong.new(
  [ "--help",      "-h", GetoptLong::NO_ARGUMENT ],
  [ "--no-commit", "-n", GetoptLong::NO_ARGUMENT ],
  [ "--file-list", "-f", GetoptLong::REQUIRED_ARGUMENT ],
  [ "--tag",       "-t", GetoptLong::REQUIRED_ARGUMENT ] )

# Parse the command line options
begin
  options.each do |opt, arg|
    case opt
      when "--help"
        RDoc::usage
        exit 0
      when "--no-commit"
        commit = false
      when "--file-list"
        file = arg
      when "--tag"
        tag = arg
      end
  end
rescue StandardError=>error_message
  puts "\nERROR: #{error_message}\n"
  RDoc::usage
  exit 1
end

# Check the arguments
if ARGV.length != 2
  puts("\nERROR: Must specify SOURCE directory and Subversion URL.")
  puts("Try \"#{File.basename($0)} --help\"\n ")
  exit 1
end
source = ARGV[0]
url    = ARGV[1]

# Begin the main error handler
begin

  # Do not want to checkout a new working copy
  # in a directory under version control
  raise "Current directory is under subversion control!" if File.directory?(".svn") 
  
  # Create a temporary working copy name
  dest = Dir.getwd+"/"+Time.now.strftime("SVN-REL.%Y-%m-%d.%H%M%S")

  # Checkout the repository
  svn = Svn_Util::Svn.new(url)
  svn.checkout(dest)

  # Update the working copy
  FileUtils.cd(dest) do
    raise "#{dest} is not an svn working copy!" unless File.directory?(".svn")
    inventory = Svn_Util::Inventory.new(file)  # Read the current file inventory
    inventory.rsync_dirs(source, dest)         # Update the working copy
    inventory.inventory                        # Re-inventory
    inventory.remove_empty_dirs                # Remove empty directory heirarchies
    inventory.report                           # Report the changes
    inventory.write_inventory(file)            # Dump updated inventory to file
    svn.add                                    # Schedule files for addition
    svn.delete                                 # Schedule files for deletion
  end

  # Commit the changes if required
  if commit
    svn.commit(tag)
    FileUtils.rm_rf(dest)
  end

# Inform user why script barfed
rescue StandardError=>error_message
  puts "\nERROR: #{error_message}\n"
  exit 1
end   


