;+
; Function to test association
; status of an SRF structure

FUNCTION Associated_SRF, SRF, $             ; Input
                         ANY_TEST=ANY_Test  ; Input keyword
;- 
 
  ; Set up error handler
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN, FALSE
  ENDIF
 
  ; Check the structure
  IF ( Is_A_SRF_Structure( SRF ) EQ FALSE ) THEN $
    MESSAGE, 'Input structure is not a SRF structure', $
             /NONAME, /NOPRINT

  ; Test association status
  Association_Status = FALSE
  IF ( KEYWORD_SET( ANY_Test ) ) THEN BEGIN
    IF ( PTR_VALID( SRF.f1_Band   ) AND $
         PTR_VALID( SRF.f2_Band   ) AND $
         PTR_VALID( SRF.npts_Band ) AND $
         PTR_VALID( SRF.Frequency ) AND $
         PTR_VALID( SRF.Response  )     ) THEN Association_Status = TRUE
  ENDIF ELSE BEGIN
    IF ( PTR_VALID( SRF.f1_Band   ) OR $
         PTR_VALID( SRF.f2_Band   ) OR $
         PTR_VALID( SRF.npts_Band ) OR $
         PTR_VALID( SRF.Frequency ) OR $
         PTR_VALID( SRF.Response  )    ) THEN Association_Status = TRUE
  ENDELSE
 
  RETURN, Association_Status
 
END ; FUNCTION Associated_SRF
