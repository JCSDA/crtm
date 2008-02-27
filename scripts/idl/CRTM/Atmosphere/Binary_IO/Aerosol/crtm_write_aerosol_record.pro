;+
FUNCTION CRTM_Write_Aerosol_Record, FileID     , $  ; Input
                                    Aerosol    , $  ; Input
                                    DEBUG=Debug     ; Optional input
;-
  ; Set up error handler
  ; --------------------
  @crtm_binary_io_error

  ; Loop over the number of aerosols
  ; --------------------------------
  FOR m = 0, N_ELEMENTS(Aerosol)-1 DO BEGIN
  
    ; Write the data dimensions
    ; -------------------------
    WRITEU, FileID, Aerosol[m].n_Layers
  
    ; Write the Aerosol data
    ; ----------------------
    WRITEU, FileID, Aerosol[m].Type, $
                   *Aerosol[m].Effective_Radius, $
                   *Aerosol[m].Concentration
                  
    IF ( KEYWORD_SET(Debug) ) THEN BEGIN
      Msg = '  Aerosol #'+STRTRIM(m+1,2)+$
            '; n_Layers='+STRTRIM(Aerosol[m].n_Layers,2)+$
            '; Type='+STRTRIM(Aerosol[m].Type,2)
      MESSAGE, Msg, /INFORMATIONAL
    ENDIF

  ENDFOR
  
  ; Done
  ; ----
  CATCH, /CANCEL
  RETURN, SUCCESS
  
END
