# Put iasi_tauprod heirarchy at
# beginning of search path
$:.unshift File.join(File.dirname(__FILE__))

require 'getoptlong'
require 'rdoc/usage'

module TauProd
  class Config

    # Define the constants
    # --------------------
    LL_SUBMIT    = "llsubmit"
    LL_ACCOUNT   = { :ccs=>"GDAS-T2O", :haze=>"JCSDA001-RES"}
    LL_TIMELIMIT = "03:00:00"
    LL_RESOURCES = "ConsumableCpus(1) ConsumableMemory(500)"

    SUBMIT_INCREMENT = 60*60*3  # seconds

    LBLRTM_HITRAN_VERSION = "LBLRTM v9.4; HITRAN 2000 + AER updates"
    DFAVG   = 0.001  # Averaging kernel width in cm^-1
    NPANELS = 1      # No. of LBL panels
    UPDIRN  = 1      # Direction flag for upwelling
    DNDIRN  = 2      # Direction flag for upwelling
    SENSOR_ID = "iasi_metopa"
    
    # Literal constants
    ZERO=0.0; ONE=1.0
    
    # Profile set info
    PROFILE_SET_INFO = { "UMBC"  => {:id => 1, :n_profiles => 48},
                         "ECMWF" => {:id => 2, :n_profiles => 52},
                         "CIMSS" => {:id => 3, :n_profiles => 32}}
                         
    # Band information. The SCNMRG frequencies are those values that,
    # for a point spacing of 0.001cm^-1, produce a number of points
    # that has only prime factors of 2, 3, and 5.
    BAND_INFO = { 1 => {:f1_scnmrg =>  605.0, :f2_scnmrg => 1253.00},
                  2 => {:f1_scnmrg => 1170.0, :f2_scnmrg => 2107.50},
                  3 => {:f1_scnmrg => 1960.0, :f2_scnmrg => 2803.75}}
    
    # Molecule information
    MOL_INFO=Hash.new
    (1..7).collect {|m| MOL_INFO[m] = {:name  => "mol#{m}",
                                       :t3tag => "mol#{m}",
                                       :cont  => [ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO]}}
    MOL_INFO[8]  = {:name  => "all_nocontinua",
                    :t3tag => "7mol",
                    :cont  => [ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO]}
    MOL_INFO[9]  = {:name  => "continua_only",
                    :t3tag => "nomol",
                    :cont  => [ONE,ONE,ONE,ONE,ONE,ONE,ONE]}
    MOL_INFO[10] = {:name  => "all_withcontinua",
                    :t3tag => "7mol",
                    :cont  => [ONE,ONE,ONE,ONE,ONE,ONE,ONE]}
    MOL_INFO[11] = {:name  => "wvo",
                    :t3tag => "wvo",
                    :cont  => [ONE,ONE,ZERO,ONE,ZERO,ZERO,ZERO]}
    MOL_INFO[12] = {:name  => "wet",
                    :t3tag => "mol1",
                    :cont  => [ONE,ONE,ZERO,ZERO,ZERO,ZERO,ZERO]}
    MOL_INFO[13] = {:name  => "dry",
                    :t3tag => "dry",
                    :cont  => [ZERO,ZERO,ONE,ZERO,ONE,ONE,ONE]}
    MOL_INFO[14] = {:name  => "ozo",
                    :t3tag => "mol3",
                    :cont  => [ZERO,ZERO,ZERO,ONE,ZERO,ZERO,ZERO]}
    MOL_INFO[15] = {:name  =>"wco",
                    :t3tag =>"nomol",
                    :cont  =>[ONE,ONE,ZERO,ZERO,ZERO,ZERO,ZERO]}


    # Angle information
    ANGLE_INFO = { 1 => {:secant => 1.00, :angle => 0.0   , :zenith => 180.000},
                   2 => {:secant => 1.25, :angle => 36.870, :zenith => 143.130},
                   3 => {:secant => 1.50, :angle => 48.190, :zenith => 131.810},
                   4 => {:secant => 1.75, :angle => 55.150, :zenith => 124.850},
                   5 => {:secant => 2.00, :angle => 60.000, :zenith => 120.000},
                   6 => {:secant => 2.25, :angle => 63.612, :zenith => 116.388},
                   7 => {:secant => 3.00, :angle => 70.529, :zenith => 109.471} }


    # Accepted command line options
    # -----------------------------
    OPTIONS = GetoptLong.new(
      [ "--help"       , "-h", GetoptLong::NO_ARGUMENT       ],
      [ "--debug"      , "-g", GetoptLong::NO_ARGUMENT       ],
      [ "--noop"       , "-n", GetoptLong::NO_ARGUMENT       ],
      [ "--angles"     , "-a", GetoptLong::REQUIRED_ARGUMENT ],
      [ "--bands"      , "-b", GetoptLong::REQUIRED_ARGUMENT ],
      [ "--co2_mr"     , "-c", GetoptLong::REQUIRED_ARGUMENT ],
      [ "--start_delay", "-i", GetoptLong::REQUIRED_ARGUMENT ],
      [ "--molid"      , "-m", GetoptLong::REQUIRED_ARGUMENT ],
      [ "--profiles"   , "-p", GetoptLong::REQUIRED_ARGUMENT ],
      [ "--queue"      , "-q", GetoptLong::REQUIRED_ARGUMENT ],
      [ "--profile_set", "-s", GetoptLong::REQUIRED_ARGUMENT ],
      [ "--t5_dir"     , "-d", GetoptLong::REQUIRED_ARGUMENT ],
      [ "--t3_id"      , "-t", GetoptLong::REQUIRED_ARGUMENT ] )


    # Begin class definitions
    # -----------------------    
    attr_reader :queue, :start_delay, :t5_dir, :t3_id, :co2_mr, :profile_set, :profiles,
                :bands, :molids, :angles, :noop, 
                :debug
                
    def initialize(debug = false)
      @queue       = "dev"                        # Processing queue
      @start_delay = 60*5                         # Batch job submission delay in seconds
      @t5_dir      = "./TAPE5_files"              # Location of the LBLRTM input files
      @t3_id       = "hitran2000_aer"             # Spectroscopic database
      @co2_mr      = 380.0                        # CO2 mixing ratio
      @profile_set = "UMBC"                       # Profile set to process
      @profiles    = (1..PROFILE_SET_INFO[@profile_set][:n_profiles]).to_a  # Profiles to process
      @bands       = BAND_INFO.keys.sort          # Bands to process
      @molids      = [1, 10, 11, 12, 13, 14, 15]  # Molecule sets to process
      @angles      = ANGLE_INFO.keys.sort         # Angles to process
      @noop        = false                        # Submit jobs by default
      @debug       = debug                        # Have a guess
    end

    def process_arguments
      begin
        OPTIONS.each do |opt, arg|
        puts opt.inspect, arg.inspect
          case opt
            when "--help"
              RDoc::usage
              exit 0
            when "--debug"
              @debug = true
            when "--noop"
              @noop = true
            when "--angles"
              @angles = parse_range(arg)
            when "--bands"
              @bands = parse_range(arg)
            when "--molid"
              @molids = parse_range(arg)
            when "--profiles"
              @profiles = parse_range(arg)
            when "--co2_mr"
              @co2_mr = arg.to_f
            when "--start_delay"
              @start_delay = (arg.to_i)*60  # Convert to seconds
            when "--queue"
              @queue = arg
            when "--profile_set"
              @profile_set = arg if PROFILE_SET_INFO.has_key?(arg)
            when "--t5_dir"
              @t5_dir = arg
            when "--t3_id"
              @t3_id = arg
          end
        end
      rescue StandardError => error_message
        puts("\nERROR: #{error_message}\n")
        RDoc::usage
        exit 1
      end
    end
    
    def display
      puts "\nTauProd config values:"
      puts "queue       : #{@queue}"
      puts "start_delay : #{@start_delay}"
      puts "t5_dir      : #{@t5_dir}"
      puts "t3_id       : #{@t3_id}"
      puts "co2_mr      : #{@co2_mr} ppmv"
      puts "profile_set : #{@profile_set}"
      puts "profiles    : #{@profiles.join(",")}"
      puts "bands       : #{@bands.join(",")}"
      puts "molids      : #{@molids.join(",")}"
      puts "angles      : #{@angles.join(",")}"
      puts "noop        : #{@noop}"
      puts "debug       : #{@debug}"
    end

  private
  
    # Method to parse input range definitions
    # e.g. 1-3,6,10-12  produces [1,2,3,6,10,11,12]
    def parse_range(input)
      input.split(",").collect do |element|
        if element =~ /(\d+)-(\d+)/
          ($1.to_i..$2.to_i).to_a
        else
          element.to_i
        end
      end.flatten.uniq.sort
    end

  end
end
