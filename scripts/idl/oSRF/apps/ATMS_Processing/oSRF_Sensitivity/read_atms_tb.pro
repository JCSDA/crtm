;+
; Procedure to read the computed ATMS channel brightness
; temperatures from an ASCII datafile
;
PRO read_atms_tb, $
  Filename, $ ; Input (must exist)
  Tb      , $ ; Output
  Channel_List = Channel_List  ; Output keyword
;-

  ; Set up error handler
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN
  ENDIF


  ; Read and reform the data
  x = ddread(filename,/quiet,type=5)
  Channel_List = LONG(REFORM(x[0,*]))
  Tb = REFORM(x[1,*])
  
END
  
