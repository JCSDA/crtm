;---------------------
PRO TauProfile__Define
;---------------------

  void = { TauProfile, $
           n_Allocates: 0, $

           ; Release and version information
           Release: 0L, $
           Version: 0L, $
           ; Dimensions
           n_Layers       : 0L, $  ; K
           n_Channels     : 0L, $  ; L
           n_Angles       : 0L, $  ; I
           n_Profiles     : 0L, $  ; M
           n_Molecule_Sets: 0L, $  ; J

           ; Sensor information
           Sensor_ID       : '', $
           WMO_Satellite_ID: 0L, $
           WMO_Sensor_ID   : 0L, $

           ; Dimension descriptor data
           Level_Pressure: PTR_NEW(), $  ; K+1
           Channel       : PTR_NEW(), $  ; L
           Angle         : PTR_NEW(), $  ; I
           Profile       : PTR_NEW(), $  ; M
           Molecule_Set  : PTR_NEW(), $  ; J

           ; Transmittances
           Tau: PTR_NEW() }  ; K x L x I x M x J

END ; PRO TauProfile_Define
