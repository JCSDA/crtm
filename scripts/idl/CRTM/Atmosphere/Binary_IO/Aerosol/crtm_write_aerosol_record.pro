;+
FUNCTION CRTM_Write_Aerosol_Record, FileID     , $  ; Input
                                    Aerosol    , $  ; Input
                                    DEBUG=Debug     ; Optional input
;-
  ; Set up error handler
  ; --------------------
  @crtm_binary_io_error

  ; Write the data dimensions
  ; -------------------------
  WRITEU, FileID, Aerosol.n_Layers
  
  ; Write the Aerosol data
  ; ----------------------
  WRITEU, FileID, Aerosol.Type, $
                 *Aerosol.Effective_Radius, $
                 *Aerosol.Concentration
                
  IF ( KEYWORD_SET(Debug) ) THEN BEGIN
    Msg = '  n_Layers='+STRTRIM(Aerosol.n_Layers,2)+$
          '; Type='+STRTRIM(Aerosol.Type,2)
    MESSAGE, Msg, /INFORMATIONAL
  ENDIF

  ; Done
  ; ----
  CATCH, /CANCEL
  RETURN, SUCCESS
  
END
