;+
FUNCTION CRTM_Allocate_Cloud, n_Layers, $  ; Input
                              Cloud        ; Output
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
  result = CRTM_Destroy_Cloud( Cloud )
  IF ( result NE SUCCESS ) THEN $
     MESSAGE, 'Error destroying Cloud structure', $
              /NONAME, /NOPRINT

  ; Allocate the intrinsic type arrays
  ; ----------------------------------
  Cloud.Effective_Radius   = PTR_NEW(DBLARR( n_Layers ))
  Cloud.Effective_Variance = PTR_NEW(DBLARR( n_Layers ))
  Cloud.Water_Content      = PTR_NEW(DBLARR( n_Layers ))

  ; Assign the dimension
  ; --------------------
  Cloud.n_Layers = n_Layers

  ; Done
  ; ----
  CATCH, /CANCEL
  RETURN, SUCCESS
  
END
