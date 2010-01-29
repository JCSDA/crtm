PRO EmisCoeff__Define
  void = { EmisCoeff, $
           Release      : 0L, $
           Version      : 0L, $
           Data_Type    : 0L, $
           n_Angles     : 0L, $
           n_Frequencies: 0L, $
           n_Wind_Speeds: 0L, $
           Angle        : PTR_NEW(), $
           Frequency    : PTR_NEW(), $
           Wind_Speed   : PTR_NEW(), $
           Emissivity   : PTR_NEW()  }  
END        
