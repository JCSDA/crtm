module TauProd
  class Config

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
      @debug             = debug
    end


    # -------------
    # Class methods
    # -------------
    # Method to load tauprod configuration
    def self.load(file_name,debug,args)
      config = new(debug)
      config.parse(File.open(file_name,"r").readlines.delete_if {|line| line=~/^\s*!|^\s*#|^\s*$/},args)
      config
    end


    # ----------------
    # Instance methods
    # ----------------
    # Method to parse tauprod config definitions
    def parse(lines,args)

      # Parse line by line
      lines.each do |line|
        name,value,description = line.split(":").each {|s| s.strip!}
        case name.downcase
          when "queue"
            @queue = args[:queue] ? args[:queue] : value
          when "bands"
            @bands = args[:bands] ? parse_range(args[:bands]) : parse_range(value)
          when "angles"
            @angles = args[:angles] ? parse_range(args[:angles]) : parse_range(value)
          when "profiles"
            @profiles = args[:profiles] ? parse_range(args[:profiles]) : parse_range(value)
          when "molids"
            @molids = args[:molids] ? parse_range(args[:molids]) : parse_range(value)
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
            @tape3_id = args[:tape3_id] ? args[:tape3_id] : value
          when "tape5_dir"
            @tape5_dir = args[:tape5_dir] ? args[:tape5_dir] : value
          when "co2mr"
            @co2mr = value.to_f
          else
            puts("Unrecognised name: [#{name}]")
        end
      end
    end

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
  end # Class
end # Module
