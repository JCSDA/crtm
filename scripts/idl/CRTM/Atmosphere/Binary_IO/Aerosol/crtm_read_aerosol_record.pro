;+
FUNCTION CRTM_Read_Aerosol_Record, FileID           , $  ; Input
                                   Aerosol          , $  ; Output
                                   DEBUG=Debug           ; Optional input
;-
  ; Set up error handler
  ; --------------------
  @crtm_binary_io_error

  ; Loop over the number of aerosols
  ; --------------------------------
  FOR m = 0, N_ELEMENTS(Aerosol)-1 DO BEGIN
  
    ; Read the data dimensions and check
    ; ----------------------------------
    n_Layers = Aerosol[m].n_Layers
    READU, FileID, n_Layers
    IF ( n_Layers NE Aerosol[m].n_Layers ) THEN $
      MESSAGE, 'Aerosol data dimensions, ' + $
               STRTRIM(n_Layers,2)+ $
               ', are inconsistent with structure definition, ' + $
               STRTRIM(Aerosol[m].n_Layers,2), $
               /NONAME, /NOPRINT
  
    ; Read the Aerosol data
    ; -------------------
    Type = Aerosol[m].Type
    READU, FileID, Type, $
                   *Aerosol[m].Effective_Radius, $
                   *Aerosol[m].Concentration
    Aerosol[m].Type = Type
                  
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
