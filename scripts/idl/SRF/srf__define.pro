;+
; SRF structure definition procedure

PRO SRF__Define
;-

  void = { SRF, $
           n_Allocates     : 0,         $  ; Allocation counter
           n_Points        : 0L,        $  ; Dimensions
           Sensor_Name     : ' ',       $  ; Sensor name
           Platform_Name   : ' ',       $  ; Platform names
           NCEP_Sensor_ID  : 0L,        $  ; Sensor ID used at NCEP
           WMO_Satellite_ID: 0L,        $  ; Sensor ID defined by WMO
           WMO_Sensor_ID   : 0L,        $  ; Sensor ID defined by WMO
           Channel         : 0L,        $  ; Sensor channel numbers
           Begin_Frequency : 0.0d0,     $  ; Begin frequency of SRF
           End_Frequency   : 0.0d0,     $  ; End frequency of SRF
           Frequency       : PTR_NEW(), $  ; SRF frequency grid
           Response        : PTR_NEW(), $  ; SRF data
           Integrated_SRF  : 0.0d0,     $  ; Integrated SRF using integration routine
           Summation_SRF   : 0.0d0      }  ; Integrated SRF using summation

END ; PRO SRF__Define
