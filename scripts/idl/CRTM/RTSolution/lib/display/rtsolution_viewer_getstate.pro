;+
; Procedure to get the RTSolution_Viewer Info state

PRO RTSolution_Viewer_GetState, $
  ID  , $          ; Input
  Info, $          ; Output
  No_Copy=No_Copy  ; Optional input
;-

  ; Get and test pointer
  WIDGET_CONTROL, ID, GET_UVALUE = InfoPtr
  IF ( PTR_VALID( InfoPtr ) EQ 0 ) THEN $
    MESSAGE, 'State Information pointer is invalid'

  ; Get state information structure
  IF ( N_ELEMENTS( *InfoPtr ) EQ 0 ) THEN $
    MESSAGE, 'State information structure is undefined'
  IF ( KEYWORD_SET( No_Copy ) ) THEN BEGIN
    Info = TEMPORARY( *InfoPtr )
  ENDIF ELSE BEGIN
    Info = *InfoPtr
  ENDELSE

  ; Debug output
  IF ( Info.Debug EQ 1 ) THEN PRINT, 'RTSolution_Viewer_GetState'

END ; RTSolution_Viewer_GetState
