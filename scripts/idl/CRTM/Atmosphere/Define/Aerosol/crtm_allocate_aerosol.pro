;+
FUNCTION CRTM_Allocate_Aerosol, n_Layers, $  ; Input
                                Aerosol      ; Output
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
  result = CRTM_Destroy_Aerosol( Aerosol )
  IF ( result NE SUCCESS ) THEN $
     MESSAGE, 'Error destroying Aerosol structure', $
              /NONAME, /NOPRINT

  ; Allocate the intrinsic type arrays
  ; ----------------------------------
  Aerosol.Effective_Radius = PTR_NEW(DBLARR( n_Layers ))
  Aerosol.Concentration    = PTR_NEW(DBLARR( n_Layers ))

  ; Assign the dimension
  ; --------------------
  Aerosol.n_Layers = n_Layers

  ; Done
  ; ----
  CATCH, /CANCEL
  RETURN, SUCCESS
  
END
