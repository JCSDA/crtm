PRO TauProfile_View, File, Debug = Debug


  ; --------------------
  ; Set up error handler
  ; --------------------

  @error_codes

  CATCH, error_status

  IF ( error_status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN
  ENDIF    


  ; ---------------
  ; Check arguments
  ; ---------------

  IF ( NOT KEYWORD_SET( Debug ) ) THEN BEGIN
    Debug = 0 
  ENDIF ELSE BEGIN
    Debug = 1
    PRINT, 'TauProfile_View'
  ENDELSE



  ;#----------------------------------------------------------------------------#
  ;#                       -- CREATE THE WIDGET DISPLAY --                      #
  ;#----------------------------------------------------------------------------#

  TauProfile_GUI, File, Debug = Debug


END
