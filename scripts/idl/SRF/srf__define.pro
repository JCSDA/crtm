;+
; SRF structure definition procedure
;
PRO SRF__Define
;
;-

  void = { SRF, $
           n_Allocates     : 0L,        $  ; Allocation counter
           Release         : 0L,        $  ; Release: this identified structure and file format
           Version         : 0L,        $  ; Version: this is just the data version
           n_Points        : 0L,        $  ; Number of spectral points (L)
           n_Bands         : 0L,        $  ; Number of frequency bands (N)
           Sensor_Id       : ' ',       $  ; Sensor identifier
           WMO_Satellite_ID: 0L,        $  ; Sensor ID defined by WMO
           WMO_Sensor_ID   : 0L,        $  ; Sensor ID defined by WMO
           Sensor_Type     : 0L,        $  ; Sensor type (MW, IR, etc)
           Channel         : 0L,        $  ; Sensor channel number
           Integrated_SRF  : 0.0d0,     $  ; Integrated SRF using integration routine
           Summation_SRF   : 0.0d0,     $  ; Integrated SRF using summation
           f1_Band         : PTR_NEW(), $  ; Band begin frequencies
           f2_Band         : PTR_NEW(), $  ; Band end   frequencies
           npts_Band       : PTR_NEW(), $  ; Number of points per band
           Frequency       : PTR_NEW(), $  ; SRF frequency grid
           Response        : PTR_NEW()  }  ; SRF data

END ; PRO SRF__Define
