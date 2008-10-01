;+
; Function to test association
; status of a SensorInfo structure

FUNCTION SensorInfo::Associated, ANY_TEST=ANY_Test, $  ; Input keyword
                                 Debug=Debug           ; Input keyword
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
      RETURN, FALSE
    ENDIF
  ENDELSE
 
  ; Test association status
  Association_Status = FALSE
  IF ( KEYWORD_SET( ANY_Test ) ) THEN BEGIN
    IF ( PTR_VALID( self.Sensor_Channel ) AND $
         PTR_VALID( self.Use_Flag       ) AND $
         PTR_VALID( self.Noise          )     ) THEN Association_Status = TRUE
  ENDIF ELSE BEGIN
    IF ( PTR_VALID( self.Sensor_Channel ) OR $
         PTR_VALID( self.Use_Flag       ) OR $
         PTR_VALID( self.Noise          )    ) THEN Association_Status = TRUE
  ENDELSE
 
  RETURN, Association_Status
 
END ; FUNCTION SensorInfo::Associated
