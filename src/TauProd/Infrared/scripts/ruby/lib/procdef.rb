#!/usr/bin/env ruby

# String method to parse value ranges
class String
  def parse_range  #(str)
    sa=[]
    self.split(",").each do |s|
      if s=~/(\d*)-(\d*)/
        sa << ($1.to_i..$2.to_i).to_a
      else
        sa << s.to_i
      end
    end
    pr=sa.flatten.uniq
  end
end

# Process Defaults class
class ProcDef
  attr_accessor :queue,:bands,:angles,:profiles,:molids,
                :profile_set_id,:profile_set_index,
                :f1_band1,:df_band,:df_avg,:df_index,
                :tape3_id,:tape5_dir,
                :co2mr
                
  MOLNAME=Hash.new
  # The first seven molecule names
  (1..7).to_a.collect {|m| MOLNAME[m]="mol#{m}"}
  # The remaining weirdo combo names
  MOLNAME[ 8]="all_nocontinuum"
  MOLNAME[ 9]="continua_only"
  MOLNAME[10]="all_withcontinua"
  MOLNAME[11]="wvo"
  MOLNAME[12]="wet"
  MOLNAME[13]="dry"
  MOLNAME[14]="ozo"
  MOLNAME[15]="wco"

  # The molid and associated continua specification
  ZERO=0.0; ONE=1.0
  MOLID=Hash.new
  (1..7).to_a.collect {|m| MOLID[m]={"name"=>"mol#{m}",
                                     "t3tag"=>".mol#{m}",
                                     "cont"=>[ZERO,   # h2o_sc
                                              ZERO,   # h2o_fc
                                              ZERO,   # co2_c
                                              ZERO,   # o3_c
                                              ZERO,   # o2_c
                                              ZERO,   # n2_c
                                              ZERO]}} # rayleigh_ext
  MOLID[8]={"name"=>"all_nocontinua",
            "t3tag"=>"",
            "cont"=>[ZERO,   # h2o_sc
                     ZERO,   # h2o_fc
                     ZERO,   # co2_c
                     ZERO,   # o3_c
                     ZERO,   # o2_c
                     ZERO,   # n2_c
                     ZERO]}  # rayleigh_ext
  MOLID[9]={"name"=>"continua_only",
            "t3tag"=>".nomol",
            "cont"=>[ONE,   # h2o_sc
                     ONE,   # h2o_fc
                     ONE,   # co2_c
                     ONE,   # o3_c
                     ONE,   # o2_c
                     ONE,   # n2_c
                     ONE]}  # rayleigh_ext
  MOLID[10]={"name"=>"all_withcontinua",
             "t3tag"=>"",
             "cont"=>[ONE,   # h2o_sc
                      ONE,   # h2o_fc
                      ONE,   # co2_c
                      ONE,   # o3_c
                      ONE,   # o2_c
                      ONE,   # n2_c
                      ONE]}  # rayleigh_ext
  MOLID[11]={"name"=>"wvo",
             "t3tag"=>".wvo",
             "cont"=>[ONE,    # h2o_sc
                      ONE,    # h2o_fc
                      ZERO,   # co2_c
                      ONE,    # o3_c
                      ZERO,   # o2_c
                      ZERO,   # n2_c
                      ZERO]}  # rayleigh_ext
  MOLID[12]={"name"=>"wet",
             "t3tag"=>".mol1",
             "cont"=>[ONE,    # h2o_sc
                      ONE,    # h2o_fc
                      ZERO,   # co2_c
                      ZERO,   # o3_c
                      ZERO,   # o2_c
                      ZERO,   # n2_c
                      ZERO]}  # rayleigh_ext
  MOLID[13]={"name"=>"dry",
             "t3tag"=>".dry",
             "cont"=>[ZERO,   # h2o_sc
                      ZERO,   # h2o_fc
                      ONE,    # co2_c
                      ZERO,   # o3_c
                      ONE,    # o2_c
                      ONE,    # n2_c
                      ONE]}   # rayleigh_ext
  MOLID[14]={"name"=>"ozo",
             "t3tag"=>".mol3",
             "cont"=>[ZERO,   # h2o_sc
                      ZERO,   # h2o_fc
                      ZERO,   # co2_c
                      ONE,    # o3_c
                      ZERO,   # o2_c
                      ZERO,   # n2_c
                      ZERO]}  # rayleigh_ext
  MOLID[15]={"name"=>"wco",
             "t3tag"=>".nomol",
             "cont"=>[ONE,    # h2o_sc
                      ONE,    # h2o_fc
                      ZERO,   # co2_c
                      ZERO,   # o3_c
                      ZERO,   # o2_c
                      ZERO,   # n2_c
                      ZERO]}  # rayleigh_ext
  
  # The processing angles
  ANGLE={1=>{"secant"=>1.00,"angle"=>0.0   ,"zenith"=>180.000},
         2=>{"secant"=>1.25,"angle"=>36.870,"zenith"=>143.130},
         3=>{"secant"=>1.50,"angle"=>48.190,"zenith"=>131.810},
         4=>{"secant"=>1.75,"angle"=>55.150,"zenith"=>124.850},
         5=>{"secant"=>2.00,"angle"=>60.000,"zenith"=>120.000},
         6=>{"secant"=>2.25,"angle"=>63.612,"zenith"=>116.388},
         7=>{"secant"=>3.00,"angle"=>70.529,"zenith"=>109.471}}

  def initialize
    @queue             = ""   # Default batch processing queue
    @bands             = []   # Bands to process
    @angles            = []   # Angles to process
    @profiles          = []   # Profiles to process
    @molids            = []   # Molecules to process
    @profile_set_id    = ""   # Profile set identifier
    @profile_set_index = nil  # Profile set index
    @f1_band1          = nil  # The begin frequency of band #1
    @df_band           = nil  # The effective bandwidth of each band
    @df_avg            = nil  # The averaging kernel frequency width.
    @df_index          = nil  # The averaging kernel frequency width index
    @tape3_id          = ""   # The spectroscopic database to use
    @tape5_dir         = ""   # The directory where the TAPE5 file are located.
    @co2mr             = nil  # Default CO2 mixing ratio in ppmv.
  end

  def read(file)
    File.open(file,"r").readlines.delete_if {|line| line=~/^\s*!|^\s*#|^\s*$/}.each do |line|
      name,value,description=line.split(":").each {|s| s.strip!}
      case name.downcase
        when "queue"
          @queue=value
        when "bands"
          @bands=value.parse_range
        when "angles"
          @angles=value.parse_range
        when "profiles"
          @profiles=value.parse_range
        when "molids"
          @molids=value.parse_range
        when "profile_set_id"
          @profile_set_id=value
        when "profile_set_index"
          @profile_set_index=value.to_i
        when "f1_band1"
          @f1_band1=value.to_f
        when "df_band"
          @df_band=value.to_f
        when "df_avg"
          @df_avg=value.to_f
        when "df_index"
          @df_index=value.to_i
        when "tape3_id"
          @tape3_id=value
        when "tape5_dir"
          @tape5_dir=value
        when "co2mr"
          @co2mr=value.to_f
        else
          puts("Unrecognised name: [#{name}]")
      end
    end
  end
  
  def display
    puts "\nTauProd ProcDef current values:"
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

end

