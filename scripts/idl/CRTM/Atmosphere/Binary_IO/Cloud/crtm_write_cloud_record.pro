;+
FUNCTION CRTM_Write_Cloud_Record, FileID     , $  ; Input
                                  Cloud      , $  ; Input
                                  DEBUG=Debug     ; Optional input
;-
  ; Set up error handler
  ; --------------------
  @crtm_binary_io_error

  ; Loop over the number of clouds
  ; ------------------------------
  FOR m = 0, N_ELEMENTS(Cloud)-1 DO BEGIN
  
    ; Write the data dimensions
    ; -------------------------
    WRITEU, FileID, Cloud[m].n_Layers
  
    ; Write the cloud data
    ; -------------------
    WRITEU, FileID, Cloud[m].Type, $
                   *Cloud[m].Effective_Radius, $
                   *Cloud[m].Effective_Variance, $
                   *Cloud[m].Water_Content
                  
    IF ( KEYWORD_SET(Debug) ) THEN BEGIN
      Msg = '  Cloud #'+STRTRIM(m+1,2)+$
            '; n_Layers='+STRTRIM(Cloud[m].n_Layers,2)+$
            '; Type='+STRTRIM(Cloud[m].Type,2)
      MESSAGE, Msg, /INFORMATIONAL
    ENDIF

  ENDFOR
  
  ; Done
  ; ----
  CATCH, /CANCEL
  RETURN, SUCCESS
  
END
