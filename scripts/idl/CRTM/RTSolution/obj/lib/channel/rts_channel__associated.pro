FUNCTION RTS_Channel::Associated, $
  ANY_TEST=ANY_Test, $  ; Input keyword
  Debug=Debug           ; Input keyword
 
  ; Set up error handler
  @rts_func_err_handler


  ; Test association status
  Association_Status = FALSE
  IF ( KEYWORD_SET( ANY_Test ) ) THEN BEGIN
    IF ( PTR_VALID( self.Upwelling_Radiance  ) AND $
         PTR_VALID( self.Layer_Optical_Depth )     ) THEN Association_Status = TRUE
  ENDIF ELSE BEGIN
    IF ( PTR_VALID( self.Upwelling_Radiance  ) OR $
         PTR_VALID( self.Layer_Optical_Depth )    ) THEN Association_Status = TRUE
  ENDELSE
 
  RETURN, Association_Status
 
END ; FUNCTION RTS_Channel::Associated
