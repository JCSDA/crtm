;+
FUNCTION CRTM_Destroy_Aerosol, Aerosol  ; In/output
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
  Aerosol.n_Layers = 0L
  
  ; Free the structure pointers
  ; ---------------------------
  PTR_FREE, Aerosol.Effective_Radius, $
            Aerosol.Concentration

  ; Done
  ; ----
  CATCH, /CANCEL
  RETURN, SUCCESS
  
END
