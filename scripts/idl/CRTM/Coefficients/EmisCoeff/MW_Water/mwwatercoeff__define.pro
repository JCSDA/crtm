;+
PRO MWwaterCoeff__Define
  void = { MWwaterCoeff, $
           Is_Allocated  : 0L, $
           Release       : 0L, $
           Version       : 0L, $
           n_Angles      : 0L, $
           n_Frequencies : 0L, $
           n_Temperatures: 0L, $
           n_Wind_Speeds : 0L, $
           Angle         : PTR_NEW(), $
           Frequency     : PTR_NEW(), $
           Temperature   : PTR_NEW(), $
           Wind_Speed    : PTR_NEW(), $
           ev            : PTR_NEW(), $
           eh            : PTR_NEW()  }
END        
;-
