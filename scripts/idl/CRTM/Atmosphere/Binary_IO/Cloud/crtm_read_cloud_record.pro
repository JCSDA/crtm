;+
FUNCTION CRTM_Read_Cloud_Record, FileID           , $  ; Input
                                 Cloud            , $  ; Output
                                 DEBUG=Debug           ; Optional input
;-
  ; Set up error handler
  ; --------------------
  @crtm_binary_io_error

  ; Loop over the number of clouds
  ; ------------------------------
  FOR m = 0, N_ELEMENTS(Cloud)-1 DO BEGIN
  
    ; Read the data dimensions and check
    ; ----------------------------------
    n_Layers = Cloud[m].n_Layers
    READU, FileID, n_Layers
    IF ( n_Layers NE Cloud[m].n_Layers ) THEN $
      MESSAGE, 'Cloud data dimensions, ' + $
               STRTRIM(n_Layers,2)+ $
               ', are inconsistent with structure definition, ' + $
               STRTRIM(Cloud[m].n_Layers,2), $
               /NONAME, /NOPRINT
  
    ; Read the cloud data
    ; -------------------
    Type = Cloud[m].Type
    READU, FileID, Type, $
                   *Cloud[m].Effective_Radius, $
                   *Cloud[m].Effective_Variance, $
                   *Cloud[m].Water_Content
    Cloud[m].Type = Type
                  
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
