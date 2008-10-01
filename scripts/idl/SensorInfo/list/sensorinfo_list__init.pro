;+
; Init function for SensorInfo_List object

FUNCTION SensorInfo_List::Init, Debug=Debug  ; Input keyword
;- 

  ; Set up error handler
  @error_codes
  IF ( KEYWORD_SET(Debug) ) THEN BEGIN
    MESSAGE, '--> Entered.', /INFORMATIONAL
  ENDIF ELSE BEGIN
    CATCH, Error_Status
    IF ( Error_Status NE 0 ) THEN BEGIN
      CATCH, /CANCEL
      MESSAGE, !ERROR_STATE.MSG, /CONTINUE
      RETURN, FAILURE
    ENDIF
  ENDELSE

  ; Set default values
  self.n_Nodes = 0L
  self.First   = PTR_NEW({SensorInfo_Node})

  CATCH, /CANCEL
  RETURN, SUCCESS
 
END ; FUNCTION SensorInfo_List::Init
