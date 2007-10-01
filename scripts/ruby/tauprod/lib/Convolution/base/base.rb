module TauProd

  # TauProd base clase for shared
  # code and constants
  class Base
  
    # ---------------------
    # Class variable access
    # ---------------------
    attr_accessor :config

  
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

    # LBLRTM/HITRAN version string
    LBLRTM_HITRAN_VERSION = "LBLRTM v9.4; HITRAN 2000 + AER updates"


    # -------------
    # Class methods
    # -------------
    # Method to replace only the first
    # occurance of the leading spaces
    # in each line of input text.
    def strip_output(text)
      text =~ /^\s+/
      leading_spaces = $&
      text = text.to_a.collect {|l| l.sub(/^#{leading_spaces}/,"")}.to_s
    end

  end
end

  
