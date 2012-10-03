;+
; Procedure to free the RTSolution_Viewer Info state

PRO RTSolution_Viewer_FreeState, ID  ; Input
;-

  ; Include error codes
  @error_codes

  ; Get and test pointer
  WIDGET_CONTROL, ID, GET_UVALUE = InfoPtr
  IF ( PTR_VALID( InfoPtr ) EQ 0 ) THEN RETURN

  ; Debug output
  IF ( (*InfoPtr).Debug EQ 1 ) THEN PRINT, 'RTSolution_Viewer_FreeState'

  ; Destroy the RTSolution_File object component
  OBJ_DESTROY, (*InfoPtr).RTS_File

  ; Free and nullify state information pointer
  PTR_FREE, InfoPtr
  InfoPtr = PTR_NEW()

END ; RTSolution_Viewer_FreeState
