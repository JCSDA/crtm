;+
; Function to test if a structure is
; an SRF data structure.

FUNCTION Is_A_SRF_Structure, SRF, $       ; Input
                             Quiet=Quiet  ; Optional input
;-
 
  ; Set up error handler
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    IF ( NOT KEYWORD_SET( Quiet ) ) THEN MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN, FALSE
  ENDIF
 
  ; Determine if structure is anything
  IF ( N_ELEMENTS( SRF ) EQ 0 ) THEN $
    MESSAGE, 'Input is undefined', /NONAME, /NOPRINT
 
  ; Determine the data type
  Type_Name = SIZE( SRF, /TNAME )
  IF ( STRUPCASE( Type_Name ) NE 'STRUCT' ) THEN $
    MESSAGE, 'Input is not a structure', /NONAME, /NOPRINT
 
  ; Determine the structure type
  Structure_Name = TAG_NAMES( SRF, /STRUCTURE_NAME )
  IF ( STRUPCASE( Structure_Name ) NE 'SRF' ) THEN $
    MESSAGE, 'Input is not a SRF structure', /NONAME, /NOPRINT
 
  ; Done
  CATCH, /CANCEL
  RETURN, TRUE
 
END ; FUNCTION Is_A_SRF_Structure
