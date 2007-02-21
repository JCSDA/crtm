;+
; Procedure to free the SRF_Viewer Info state

PRO SRF_Viewer_FreeState, ID  ; Input
;-

  ; Include error codes
  @error_codes

  ; Get and test pointer
  WIDGET_CONTROL, ID, GET_UVALUE = InfoPtr
  IF ( PTR_VALID( InfoPtr ) EQ 0 ) THEN RETURN

  ; Debug output
  IF ( (*InfoPtr).Debug EQ 1 ) THEN PRINT, 'SRF_Viewer_FreeState'

  ; Free the SRF data pointer
  IF ( Is_A_SRF_Structure( *(*InfoPtr).SRF ) EQ TRUE ) THEN $
      Error_Status = Destroy_SRF( *(*InfoPtr).SRF )

  ; Free state information pointer
  PTR_FREE, InfoPtr

END ; SRF_Viewer_FreeState
