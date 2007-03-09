module TauProd
  class Config

    # ---------------
    # Class constants
    # ---------------
    # Literal constants
    ZERO=0.0; ONE=1.0

    # The molecular species information. The
    # continua information is in order of:
    # H2O self, H2O foreign, CO2, O3, O2, N2, Rayleigh extinction
    MOL_INFO=Hash.new
    (1..7).to_a.collect {|m| MOL_INFO[m] = {:name  => "mol#{m}",
                                            :t3tag => ".mol#{m}",
                                            :cont  => [ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO]}}
    MOL_INFO[8]  = {:name  => "all_nocontinua",
                    :t3tag => "",
                    :cont  => [ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO]}
    MOL_INFO[9]  = {:name  => "continua_only",
                    :t3tag => ".nomol",
                    :cont  => [ONE,ONE,ONE,ONE,ONE,ONE,ONE]}
    MOL_INFO[10] = {:name  => "all_withcontinua",
                    :t3tag => "",
                    :cont  => [ONE,ONE,ONE,ONE,ONE,ONE,ONE]}
    MOL_INFO[11] = {:name  => "wvo",
                    :t3tag => ".wvo",
                    :cont  => [ONE,ONE,ZERO,ONE,ZERO,ZERO,ZERO]}
    MOL_INFO[12] = {:name  => "wet",
                    :t3tag => ".mol1",
                    :cont  => [ONE,ONE,ZERO,ZERO,ZERO,ZERO,ZERO]}
    MOL_INFO[13] = {:name  => "dry",
                    :t3tag => ".dry",
                    :cont  => [ZERO,ZERO,ONE,ZERO,ONE,ONE,ONE]}
    MOL_INFO[14] = {:name  => "ozo",
                    :t3tag => ".mol3",
                    :cont  => [ZERO,ZERO,ZERO,ONE,ZERO,ZERO,ZERO]}
    MOL_INFO[15] = {:name  =>"wco",
                    :t3tag =>".nomol",
                    :cont  =>[ONE,ONE,ZERO,ZERO,ZERO,ZERO,ZERO]}

    # The processing angles information
    ANGLE_INFO = { 1 => {:secant => 1.00, :angle => 0.0   , :zenith => 180.000},
                   2 => {:secant => 1.25, :angle => 36.870, :zenith => 143.130},
                   3 => {:secant => 1.50, :angle => 48.190, :zenith => 131.810},
                   4 => {:secant => 1.75, :angle => 55.150, :zenith => 124.850},
                   5 => {:secant => 2.00, :angle => 60.000, :zenith => 120.000},
                   6 => {:secant => 2.25, :angle => 63.612, :zenith => 116.388},
                   7 => {:secant => 3.00, :angle => 70.529, :zenith => 109.471} }


    # ---------------------
    # Class variable access
    # ---------------------
    attr_accessor :queue              # Default batch processing queue
    attr_accessor :bands              # Bands to process
    attr_accessor :angles             # Angles to process
    attr_accessor :profiles           # Profiles to process
    attr_accessor :molids             # Molecules to process
    attr_accessor :profile_set_id     # Profile set identifier
    attr_accessor :profile_set_index  # Profile set index
    attr_accessor :f1_band1           # The begin frequency of band #1
    attr_accessor :df_band            # The effective bandwidth of each band
    attr_accessor :df_avg             # The averaging kernel frequency width.
    attr_accessor :df_index           # The averaging kernel frequency width index
    attr_accessor :tape3_id           # The spectroscopic database to use
    attr_accessor :tape5_dir          # The directory where the TAPE5 file are located.
    attr_accessor :co2mr              # Default CO2 mixing ratio in ppmv.
    attr_accessor :debug              # Have a guess


    # -----------------
    # Class constructor    
    # -----------------
    def initialize(debug=false)
      @queue             = ""  
      @bands             = []  
      @angles            = []  
      @profiles          = []  
      @molids            = []  
      @profile_set_id    = ""  
      @profile_set_index = nil 
      @f1_band1          = nil 
      @df_band           = nil 
      @df_avg            = nil 
      @df_index          = nil 
      @tape3_id          = ""  
      @tape5_dir         = ""  
      @co2mr             = nil 
      @debug             =debug
    end


    # -------------
    # Class methods
    # -------------
    # Method to load tauprod configuration
    def self.load(file_name,debug=false)
      config = new(debug)
      config.parse(File.open(file_name,"r").readlines.delete_if {|line| line=~/^\s*!|^\s*#|^\s*$/})
      config
    end


    # ----------------
    # Instance methods
    # ----------------
    # Method to parse tauprod config definitions
    def parse(lines)

      # Parse line by line
      lines.each do |line|
        name,value,description = line.split(":").each {|s| s.strip!}
        case name.downcase
          when "queue"
            @queue = value
          when "bands"
            @bands = parse_range(value)
          when "angles"
            @angles = parse_range(value)
          when "profiles"
            @profiles = parse_range(value)
          when "molids"
            @molids = parse_range(value)
          when "profile_set_id"
            @profile_set_id = value
          when "profile_set_index"
            @profile_set_index = value.to_i
          when "f1_band1"
            @f1_band1 = value.to_f
          when "df_band"
            @df_band = value.to_f
          when "df_avg"
            @df_avg = value.to_f
          when "df_index"
            @df_index = value.to_i
          when "tape3_id"
            @tape3_id = value
          when "tape5_dir"
            @tape5_dir = value
          when "co2mr"
            @co2mr = value.to_f
          else
            puts("Unrecognised name: [#{name}]")
        end
      end
    end
    
    # Method to display config information
    def display
      puts "\nTauProd config current values:"
      puts "queue             : #{@queue}"
      puts "bands             : #{@bands.join(" ")}"
      puts "angles            : #{@angles.join(" ")}"
      puts "profiles          : #{@profiles.join(" ")}"
      puts "molids            : #{@molids.join(" ")}"
      puts "profile_set_id    : #{@profile_set_id}"
      puts "profile_set_index : #{@profile_set_index}"
      puts "f1_band1          : #{@f1_band1}cm^-1"
      puts "df_band           : #{@df_band}cm^-1"
      puts "df_avg            : #{@df_avg}cm^-1"
      puts "df_index          : #{@df_index}"
      puts "tape3_id          : #{@tape3_id}"
      puts "tape5_dir         : #{@tape5_dir}"
      puts "co2mr             : #{@co2mr}ppmv"
    end


    # ---------------
    # Private methods
    private
    # ---------------
    # Method to parse input range definitions
    # e.g. 1-3,6,10-12  produces [1,2,3,6,10,11,12]
    def parse_range(str)
      sa=[]
      str.split(",").each do |s|
        if s=~/(\d*)-(\d*)/
          sa << ($1.to_i..$2.to_i).to_a
        else
          sa << s.to_i
        end
      end
      sa.flatten.uniq
    end
    
  end # Class
end # Module
