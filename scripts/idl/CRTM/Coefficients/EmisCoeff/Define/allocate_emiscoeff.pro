;+
FUNCTION Allocate_EmisCoeff, n_Angles     , $ ; Input
                             n_Frequencies, $ ; Input
                             n_Wind_Speeds, $ ; Input
                             EmisC            ; Output
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
  result = Destroy_EmisCoeff( EmisC )
  IF ( result NE SUCCESS ) THEN $
     MESSAGE, 'Error destroying EmisC structure', $
              /NONAME, /NOPRINT
              
  ; Allocate the arrays
  EmisC.Angle      = PTR_NEW(DBLARR( n_Angles ))
  EmisC.Frequency  = PTR_NEW(DBLARR( n_Frequencies ))
  EmisC.Wind_Speed = PTR_NEW(DBLARR( n_Wind_Speeds ))
  EmisC.Emissivity = PTR_NEW(DBLARR( n_Angles, n_Frequencies, n_Wind_Speeds ))
  
  ; Assign the dimensions
  ; ---------------------
  EmisC.n_Angles      = n_Angles
  EmisC.n_Frequencies = n_Frequencies
  EmisC.n_Wind_Speeds = n_Wind_Speeds
  
  ; Done
  ; ----
  CATCH, /CANCEL
  RETURN, SUCCESS
  
END
