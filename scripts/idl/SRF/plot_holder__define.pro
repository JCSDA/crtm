;+
; plot_SRF structure definition procedure
; 
;-
PRO plot_holder__define


  void = { plot_holder,                     $ 
           n_Allocates     : 0L,            $ ;   Allocate counter
           Infile          : ' ',           $ ;   Infile
           Sensor_Id       : ' ',           $ ;   sensor id
           Channel_Name    : ' ',           $ ;   channel name 
           f               : PTR_NEW(),     $ ;   new frequency grid
           r               : PTR_NEW(),     $ ;   new response grid
           orig_f          : PTR_NEW(),     $ ;   original frequency
           orig_r          : PTR_NEW(),     $ ;   original response
           v1              : 0.0d0,         $ ;   cutoff#1 frequency
           v2              : 0.0d0,         $ ;   cutoff#2 frequency
           f0              : 0.0d0,         $ ;   central frequency
           Percent_Removed : 0.0d0,         $ ;   percent removed
           Negative_Count  : 0L             } ;   negative count}
           
END           
