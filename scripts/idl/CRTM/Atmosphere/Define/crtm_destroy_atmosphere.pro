;+
FUNCTION CRTM_Destroy_Atmosphere, Atm  ; In/output
;-
  ; Set up error handler
  ; --------------------
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN, FAILURE
  ENDIF    

  ; Reinitialise the dimensions
  ; ---------------------------
  Atm.n_Layers    = 0L
  Atm.n_Absorbers = 0L
  Atm.n_Clouds    = 0L
  Atm.n_Aerosols  = 0L
  
  ; Free the structure pointers
  ; ---------------------------
  PTR_FREE, Atm.Absorber_ID   , $
            Atm.Absorber_Units, $
            Atm.Level_Pressure, $
            Atm.Pressure      , $
            Atm.Temperature   , $
            Atm.Absorber
  
  ; Free the Cloud structure array if required
  ; ------------------------------------------
  IF ( PTR_VALID( Atm.Cloud ) ) THEN BEGIN
    n_Clouds = N_ELEMENTS( *Atm.Cloud )
    FOR n = 0, n_Clouds - 1 DO BEGIN
      result = CRTM_Destroy_Cloud( (*Atm.Cloud)[n] )
      IF ( result NE SUCCESS ) THEN $
        MESSAGE, 'Error destroying Atmosphere Cloud element '+STRTRIM(n+1,2), $
                 /NONAME, /NOPRINT
    ENDFOR
  ENDIF
  PTR_FREE, Atm.Cloud

  ; Free the Aerosol structure array if required
  ; --------------------------------------------
  IF ( PTR_VALID( Atm.Aerosol ) ) THEN BEGIN
    n_Aerosols = N_ELEMENTS( *Atm.Aerosol )
    FOR n = 0, n_Aerosols - 1 DO BEGIN
      result = CRTM_Destroy_Aerosol( (*Atm.Aerosol)[n] )
      IF ( result NE SUCCESS ) THEN $
        MESSAGE, 'Error destroying Atmosphere Aerosol element '+STRTRIM(n+1,2), $
                 /NONAME, /NOPRINT
    ENDFOR
  ENDIF
  PTR_FREE, Atm.Aerosol

  ; Done
  ; ----
  CATCH, /CANCEL
  RETURN, SUCCESS
  
END
