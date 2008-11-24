# Put CRTM_Lib heirarchy at
# beginning of search path
$:.unshift File.join(File.dirname(__FILE__))

require 'getoptlong'
require 'rdoc/usage'

module CRTM_Lib
  class Config

    # Define constants and defaults
    # -----------------------------
    # The CRTM source root location
    CRTM_SOURCE_ROOT = ENV['CRTM_SOURCE_ROOT']
    
    # Default install directory
    INSTALL_DIR = ENV['HOME']+"/local/CRTM"

    # The library filename
    LIB_FILE = "libCRTM.a"
        
    # Build targets
    PROD_TARGETS  = ["lahey","pgi","gfortran"]
    DEBUG_TARGETS = PROD_TARGETS.collect {|t| t+"_debug"}
    TARGETS = PROD_TARGETS + DEBUG_TARGETS
    
    # Accepted command line options
    OPTIONS = GetoptLong.new(
      [ "--help"       , "-h", GetoptLong::NO_ARGUMENT       ],
      [ "--debug"      , "-g", GetoptLong::NO_ARGUMENT       ],
      [ "--no-build"   , "-b", GetoptLong::NO_ARGUMENT       ],
      [ "--no-link"    , "-l", GetoptLong::NO_ARGUMENT       ],
      [ "--install-dir", "-i", GetoptLong::REQUIRED_ARGUMENT ],
      [ "--crtm-dir"   , "-c", GetoptLong::REQUIRED_ARGUMENT ] )
      
    # Begin class definitions
    # -----------------------
    attr_reader :targets, :build, :link,
                :install_dir, :crtm_dir, :lib_dir, :include_dir,
                :lib_file,
                :debug
    
    def initialize(debug = false)
      @targets     = TARGETS           # Process everything
      @install_dir = INSTALL_DIR       # Default install directory
      @build       = true              # Build everything
      @link        = true              # Link everything
      @crtm_dir    = CRTM_SOURCE_ROOT  # CRTM source root directory
      @debug       = debug
      @lib_dir     = ""
      @include_dir = ""
      @lib_file    = LIB_FILE
    end

    def process_arguments
      # Process the command line options
      begin
        OPTIONS.each do |opt, arg|
          case opt
            when "--help"
              puts("\nTargets\n-------\n")
              KNOWN_TARGETS.each {|t| puts("#{t}")}
              RDoc::usage(0)
            when "--debug"
              @debug = true
            when "--no-build"
              @build = false
            when "--no-link"
              @link = false
            when "--install-dir"
              @install_dir = File.expand_path(arg)
            when "--crtm-dir"
              @crtm_dir = File.expand_path(arg)
          end
        end
      rescue ArgumentError => error_message
        puts("ERROR: #{error_message}")
        puts("Try \"#{script_name} --help\"\n ")
        exit 1
      end
      # Process the command line arguments
      if !ARGV.empty?
        targets = ARGV.uniq.collect {|t| t.downcase}
        @targets = TARGETS & targets
        invalid_targets = targets - TARGETS
        puts("\nIgnoring invalid target(s): #{invalid_targets.join(",")}") unless invalid_targets.empty? 
      end
      # Create the lib and include directory names
      @lib_dir     = @install_dir + "/lib"
      @include_dir = @install_dir + "/include"
    end

    def display
      puts("\nCRTM_Lib config values:")
      puts("targets     : #{@targets.join(",")}")
      puts("crtm_dir    : #{@crtm_dir}")
      puts("install_dir : #{@install_dir}")
      puts("build       : #{@build}")
      puts("link        : #{@link}")
      puts("debug       : #{@debug}\n\n")
    end    
  end
end
