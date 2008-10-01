;+
; Procedure to cleanup a SensorInfo structure

PRO SensorInfo::Cleanup, Debug=Debug  ; Input keyword
;-
 
  ; Set up
  ; ------
  ; Include SensorInfo parameters
  @sensorinfo_parameters
  
  ; error handler
  @error_codes
  IF ( KEYWORD_SET(Debug) ) THEN BEGIN
    MESSAGE, '--> Entered.', /INFORMATIONAL
    MsgSwitch = 0
  ENDIF ELSE BEGIN
    CATCH, Error_Status
    IF ( Error_Status NE 0 ) THEN BEGIN
      CATCH, /CANCEL
      MESSAGE, !ERROR_STATE.MSG, /CONTINUE
      RETURN
    ENDIF
    MsgSwitch = 1
  ENDELSE


  ; Deallocate pointers, and clear scalars
  ; --------------------------------------
  Result = self->Destroy(Debug=Debug)
  IF ( Result NE SUCCESS ) THEN $
    MESSAGE, 'Error destroying SensorInfo structure', NONAME=MsgSwitch, NOPRINT=MsgSwitch

  ; Done
  Done:
  CATCH, /CANCEL

END ; PRO SensorInfo::Cleanup
