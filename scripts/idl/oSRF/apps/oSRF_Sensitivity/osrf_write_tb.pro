;+
; Procedure to write the computed channel brightness
; temperatures to an ASCII datafile
;
PRO oSRF_Write_Tb, $
  Filename, $ ; Input (If exists, overwritten)
  Tb      , $ ; Input
  Channel_List = channel_list, $  ; Input keyword (Default = [1,2,...,n_Channels] from SensorInfo)
  Debug = debug
;-

  ; Setup
  COMPILE_OPT HIDDEN
  @osrf_pro_err_handler
  ; ...Check keywords
  n_channels = N_ELEMENTS(Tb)
  IF ( N_ELEMENTS(channel_list) GT 0 ) THEN BEGIN
    sensor_channel = channel_list
    IF ( N_ELEMENTS(sensor_channel) NE n_channels ) THEN $
      MESSAGE, "Inconsistent Channel_List", /NONAME, /NOPRINT
  ENDIF ELSE BEGIN
    sensor_channel = LINDGEN(n_channels) + 1L
  ENDELSE
  idx = SORT(sensor_channel)

  
  ; Open the output filename
  OPENW, lun, Filename, /GET_LUN, WIDTH=200
  

  ; Write the data
  ; ...Header
  PRINTF, lun, "! Ch#     Tb"
  ; ...Data
  FOR l = 0, N_ELEMENTS(Tb)-1 DO $
    PRINTF, lun, sensor_channel[idx[l]], Tb[idx[l]], FORMAT='(2x,i3,1x,f11.7)'


  ; Done
  FREE_LUN, lun
  
END
  
