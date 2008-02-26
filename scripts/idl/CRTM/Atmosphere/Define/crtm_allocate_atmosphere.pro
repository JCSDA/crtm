;+
FUNCTION CRTM_Allocate_Atmosphere, n_Layers   , $  ; Input
                                   n_Absorbers, $  ; Input
                                   n_Clouds   , $  ; Input
                                   n_Aerosols , $  ; Input
                                   Atm             ; Output
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

  ; Destroy the structure first
  ; ---------------------------
  result = CRTM_Destroy_Atmosphere( Atm )
  IF ( result NE SUCCESS ) THEN $
     MESSAGE, 'Error destroying Atmosphere structure', $
              /NONAME, /NOPRINT

  ; Allocate the intrinsic type arrays
  ; ----------------------------------
  Atm.Absorber_ID    = PTR_NEW(LONARR( n_Absorbers ))
  Atm.Absorber_Units = PTR_NEW(LONARR( n_Absorbers ))
  Atm.Level_Pressure = PTR_NEW(DBLARR( n_Layers+1 ))
  Atm.Pressure       = PTR_NEW(DBLARR( n_Layers ))
  Atm.Temperature    = PTR_NEW(DBLARR( n_Layers ))
  Atm.Absorber       = PTR_NEW(DBLARR( n_Layers, n_Absorbers ))
  
  ; Allocate the cloud and aerosol structure arrays
  ; -----------------------------------------------
  IF ( n_Clouds GT 0 ) THEN BEGIN
    Atm.Cloud = PTR_NEW(REPLICATE({CRTM_Cloud}, n_Clouds))
    FOR i = 0, n_Clouds-1 DO BEGIN
      result = CRTM_Allocate_Cloud( n_Layers, *Atm.Cloud )
    ENDFOR
  ENDIF
  IF ( n_Aerosols   GT 0 ) THEN BEGIN
    Atm.Aerosol = PTR_NEW(REPLICATE({CRTM_Aerosol}, n_Aerosols))
    FOR i = 0, n_Aerosols-1 DO BEGIN
      result = CRTM_Allocate_Aerosol( n_Layers, *Atm.Aerosol )
    ENDFOR
  ENDIF
  
  ; Assign the dimensions
  ; ---------------------
  Atm.n_Layers    = n_Layers
  Atm.n_Absorbers = n_Absorbers
  Atm.n_Clouds    = n_Clouds
  Atm.n_Aerosols  = n_Aerosols
  
  ; Done
  ; ----
  CATCH, /CANCEL
  RETURN, SUCCESS
  
END
