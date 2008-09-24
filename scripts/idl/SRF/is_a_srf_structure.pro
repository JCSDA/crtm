;+
; Function to test if a structure is
; an SRF data structure.

FUNCTION Is_A_SRF_Structure, SRF, $       ; Input
                             Quiet=Quiet  ; Optional input
;-
 
  ; Set up
  ; ------
  ; Error handler
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    IF ( NOT KEYWORD_SET(Quiet) ) THEN MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN, FALSE
  ENDIF
  
  ; Only test if structure is anything
  ; ----------------------------------
  IF ( N_ELEMENTS(SRF) GT 0 ) THEN BEGIN
     ; Determine the data type
    Type_Name = SIZE(SRF, /TNAME)
    IF ( STRUPCASE(Type_Name) NE 'STRUCT' ) THEN $
      MESSAGE, 'Input is not a structure', /NONAME, /NOPRINT
    ; Determine the structure type
    Structure_Name = TAG_NAMES(SRF, /STRUCTURE_NAME)
    IF ( STRUPCASE(Structure_Name) NE 'SRF' ) THEN $
      MESSAGE, 'Input is not a SRF structure', /NONAME, /NOPRINT
    ; It is an SRF structure
    Result = TRUE
  ENDIF ELSE BEGIN
    ; It is not anything
    Result = FALSE
  ENDELSE
 
  ; Done
  ; ----
  CATCH, /CANCEL
  RETURN, Result
 
END ; FUNCTION Is_A_SRF_Structure
