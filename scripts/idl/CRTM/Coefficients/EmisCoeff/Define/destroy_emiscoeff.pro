;+
FUNCTION Destroy_EmisCoeff, EmisC; In/output
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
  EmisC.n_Angles      = 0L
  EmisC.n_Frequencies = 0L
  EmisC.n_Wind_Speeds = 0L
  
  ; Free the structure pointers
  ; ---------------------------
  PTR_FREE, EmisC.Angle     , $
            EmisC.Frequency , $
            EmisC.Wind_Speed, $
            EmisC.Emissivity
            
  ; Done
  ; ----
  CATCH, /CANCEL
  RETURN, SUCCESS
  
END
