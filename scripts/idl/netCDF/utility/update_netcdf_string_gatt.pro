;+
;
; Procedure to update a netCDF file string global attribute.
;
PRO Update_netCDF_String_GAtt, $
  NCfile       , $  ; Input. File to modify.
  Name         , $  ; Input. Name of GAtt to update
  Update       , $  ; Input. String to prepend/replace
  New     = New, $  ; Optional Input. Set keyword if attribute doesn't yet exist.
  Replace = Replace ; Optional input. Set keyword to replace, no prepend. Ignored if "New" keyword set.
;-

  ; Set up error handler
  @error_codes
  CATCH, err_stat
  IF ( err_stat NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    IF ( N_ELEMENTS(NCid) GT 0 ) THEN NCDF_CLOSE, NCid
    RETURN
  ENDIF
  
  ; Process keywords
  Old     = ~ KEYWORD_SET(New)
  Prepend = ~ KEYWORD_SET(Replace)

  ; Update the attribute
  NCid = NCDF_OPEN(NCfile,/WRITE)
  IF ( Old AND Prepend ) THEN BEGIN
    NCDF_ATTGET, NCid, /GLOBAL, Name, GAtt
    GAtt = STRTRIM(Update,2) + '; ' + STRING(GAtt)
  ENDIF ELSE BEGIN
    GAtt = STRTRIM(Update,2)
  ENDELSE
  NCDF_CONTROL, NCid, /REDEF
  NCDF_ATTPUT, NCid, /GLOBAL, NAME, GAtt
  NCDF_CLOSE, NCid
END
