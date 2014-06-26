# Put LBLrun heirarchy at
# beginning of search path
$:.unshift File.join(File.dirname(__FILE__))

require 'optparse'

module LBLrun
  class Config

    # Define constants and defaults
    LBL_ROOT = ENV['LBL_ROOT']
    
      
    # Begin class definitions
    attr_reader :debug
    
    def initialize(debug = false)
      @debug = debug
    end

    def process_arguments

      # Command line option processing
      options = {}

      # ...Specify the options
      opts = OptionParser.new do |opts|
        opts.banner = "Usage: lblrun [options] lbl-type input-file\n$Revision$"
        
        options[:debug] = false
        opts.on("-d", "--debug", "Turns on debug mode") do |d|
          options[:debug] = true
        end
        
        options[:results_dir] = "xdefault"
        opts.on("-r", "--results-dir DIR", "Specify the subdirectory in which results will be placed.") do |dir|
          options[:results_dir] = dir
        end
        
        options[:spectroscopy_dir] = "xdefault"
        opts.on("-s", "--spectroscopy-dir DIR", "Specify the directory in which the LBL program spectroscopy file resides.") do |dir|
          options[:spectroscopy_dir] = dir
        end
        
        options[:spectroscopy_filename] = "xdefault"
        opts.on("-t", "--spectroscopy-filename FILE", "Specify the name of the LBL program spectroscopy file.") do |file|
          options[:spectroscopy_filename] = file
        end
        
        options[:emissivity_filename] = "xdefault"
        opts.on("-e", "--emissivity-filename FILE", "Specify the name of the surface emissivity file.") do |file|
          options[:emissivity_filename] = file
        end
        
        options[:reflectivity_filename] = "xdefault"
        opts.on("-p", "--reflectivity-filename FILE", "Specify the name of the surface reflectivity file.") do |file|
          options[:relfectivity_filename] = file
        end
        
        options[:sfcoptics_dir] = "xdefault"
        opts.on("-b", "--sfcoptics-dir DIR", "Specify the directory in which the surface emissivity and reflectivity data files reside.") do |dir|
          options[:sfcoptics_dir] = dir
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


      # Command line argument processing
      begin
        # ...Extract the arguments
        raise "Must specify lbl-type and input-file." if ARGV.length < 2
        @lbl_type   = ARGV[0].downcase
        @input_file = ARGV[1]
        # ...Check them
        
        # THIS IS WHERE THE CHECKING STUFF GOES
        
      rescue Exception => error_message
        puts("ERROR --> #{error_message}")
        exit(1)
      end   

    end

    def display
      puts("\nLBLrun config values:")
      puts("debug : #{@debug}\n\n")
    end    
  end
end
