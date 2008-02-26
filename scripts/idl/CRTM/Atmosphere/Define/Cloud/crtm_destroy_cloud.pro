;+
FUNCTION CRTM_Destroy_Cloud, Cloud  ; In/output
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
  Cloud.n_Layers = 0L
  
  ; Free the structure pointers
  ; ---------------------------
  PTR_FREE, Cloud.Effective_Radius, $
            Cloud.Effective_Variance, $
            Cloud.Water_Content

  ; Done
  ; ----
  CATCH, /CANCEL
  RETURN, SUCCESS
  
END
