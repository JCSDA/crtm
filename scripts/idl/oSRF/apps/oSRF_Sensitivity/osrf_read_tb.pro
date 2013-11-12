;+
; Procedure to read the computed channel brightness
; temperatures from an ASCII datafile
;
PRO oSRF_Read_tb, $
  Filename, $ ; Input (must exist)
  Tb      , $ ; Output
  Channel_List = channel_list, $  ; Output keyword
  Debug = debug
;-

  ; Setup
  COMPILE_OPT HIDDEN
  @osrf_pro_err_handler


  ; Read and reform the data
  x = ddread(Filename, /quiet, type=5)
  channel_list = LONG(REFORM(x[0,*]))
  Tb = REFORM(x[1,*])
  
END
  
