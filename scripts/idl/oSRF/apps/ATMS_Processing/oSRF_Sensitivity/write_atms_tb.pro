;+
; Procedure to write the computed ATMS channel brightness
; temperatures to an ASCII datafile
;
PRO write_atms_tb, $
  Filename, $ ; Input (If exists, overwritten)
  Tb      , $ ; Input
  Channel_List = Channel_List  ; Input keyword (Default = [1,2,...,n_Channels] from SensorInfo)
;-

  ; Set up error handler
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN
  ENDIF


  n_Channels = N_ELEMENTS(Tb)
  IF ( N_ELEMENTS(Channel_List) ) THEN BEGIN
    Sensor_Channel = Channel_List
    IF ( N_ELEMENTS(Sensor_Channel) NE n_Channels ) THEN $
      MESSAGE, "Inconsistent Channel_List", /NONAME, /NOPRINT
  ENDIF ELSE BEGIN
    Sensor_Channel = LINDGEN(n_Channels) + 1L
  ENDELSE
  
  idx = SORT(Sensor_Channel)
  
  ; Open the output filename
  OPENW, lun, Filename, /GET_LUN, WIDTH=200
  

  ; Write the data
  ; ...Header
  PRINTF, lun, "! Ch#     Tb"
  ; ...Data
  FOR l = 0, N_ELEMENTS(Tb)-1 DO $
    PRINTF, lun, Sensor_Channel[idx[l]], Tb[idx[l]], FORMAT='(2x,i3,1x,f11.7)'


  ; Done
  FREE_LUN, lun
  
END
  
