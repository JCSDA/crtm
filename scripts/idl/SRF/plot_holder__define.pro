;+
; plot_SRF structure definition procedure
; 
;-
PRO plot_holder__define


  void = { plot_holder,                     $ 
           n_Allocates     :  0L,           $ ;   Allocate counter
           Infile          : ' ',           $ ;   Infile
           Sensor_Id       : ' ',           $ ;   sensor id
           Sensor_Type     :  0L,           $ ;   sensor type
           Channel_Name    : ' ',           $ ;   channel name 
           n_bands         :  0L,           $ ;   specify the number of bands
           f               : PTR_NEW(),     $ ;   new frequency grid
           r               : PTR_NEW(),     $ ;   new response grid
           orig_f          : PTR_NEW(),     $ ;   original frequency
           orig_r          : PTR_NEW(),     $ ;   original response
           hmv             : PTR_NEW(),     $ ;   begin/end half_max frequencies
           f0              : PTR_NEW(),     $ ;   Central Frequency for the IR
           f0_doc          : PTR_NEW(),     $ ;   Documented Central Frequency
           f0_hm           : PTR_NEW(),     $ ;   Central Frequencies using hm end points
           f0_raw          : PTR_NEW(),     $ ;   Central Frequencies for raw data 
           v1              : 0.0d0,         $ ;   cutoff#1 frequency
           v2              : 0.0d0,         $ ;   cutoff#2 frequency
           Percent_Removed : 0.0d0,         $ ;   percent removed
           Negative_Count  : 0L             } ;   negative count}           
END           
