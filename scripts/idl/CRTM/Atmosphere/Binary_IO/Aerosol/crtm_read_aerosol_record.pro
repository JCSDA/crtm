;+
FUNCTION CRTM_Read_Aerosol_Record, FileID           , $  ; Input
                                   Aerosol          , $  ; Output
                                   ALLOCATE=Allocate, $  ; Optional input
                                   DEBUG=Debug           ; Optional input
;-
  ; Set up error handler
  ; --------------------
  @crtm_binary_io_error

  ; Read the data dimensions
  ; ------------------------
  n_Layers = 0L
  READU, FileID, n_Layers
  
  ; Allocate the Aerosol structure if required
  ; ----------------------------------------
  IF ( KEYWORD_SET(Allocate) ) THEN BEGIN
    result = CRTM_Allocate_Aerosol( n_Layers, $
                                    Aerosol   )
    IF ( result NE SUCCESS ) THEN $
      MESSAGE, 'Error allocating Aerosol structure', $
               /NONAME, /NOPRINT
  ENDIF ELSE BEGIN
    IF ( n_Layers NE Aerosol.n_Layers ) THEN $
      MESSAGE, 'Aerosol data dimensions, ' + $
               STRTRIM(n_Layers,2)+ $
               ', are inconsistent with structure definition, ' + $
               STRTRIM(Aerosol.n_Layers,2), $
               /NONAME, /NOPRINT
  ENDELSE
  
  ; Read the Aerosol data
  ; -------------------
  Type = Aerosol.Type
  READU, FileID, Type, $
                 *Aerosol.Effective_Radius, $
                 *Aerosol.Concentration
  Aerosol.Type = Type
                
  IF ( KEYWORD_SET(Debug) ) THEN BEGIN
    Msg = '  n_Layers='+STRTRIM(n_Layers,2)+$
          '; Type='+STRTRIM(Type,2)
    MESSAGE, Msg, /INFORMATIONAL
  ENDIF
  
  ; Done
  ; ----
  CATCH, /CANCEL
  RETURN, SUCCESS
  
END
