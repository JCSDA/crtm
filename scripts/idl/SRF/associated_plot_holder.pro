;
; Function to test 
; association status
; of a plot_holder structure

FUNCTION Associated_plot_holder, plot_holder,       $    ; Input
                                 ANY_TEST=ANY_Test       ; Input keyword
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
  IF ( Is_A_plot_holder_Structure( plot_holder ) EQ FALSE ) THEN $
    MESSAGE, 'Input structure is not a plot_holder structure', $
             /NONAME, /NOPRINT

  ; Test association status
  Association_Status = FALSE
  IF ( KEYWORD_SET( ANY_Test ) ) THEN BEGIN
    IF ( PTR_VALID( plot_holder.f      ) AND $
         PTR_VALID( plot_holder.r      ) AND $
         PTR_VALID( plot_holder.orig_f ) AND $
         PTR_VALID( plot_holder.orig_r )     ) THEN Association_Status = TRUE
  ENDIF ELSE BEGIN
    IF ( PTR_VALID( plot_holder.f      ) OR $
         PTR_VALID( plot_holder.r      ) OR $
         PTR_VALID( plot_holder.orig_f ) OR $
         PTR_VALID( plot_holder.orig_r )    ) THEN Association_Status = TRUE         
  ENDELSE
   
  RETURN, Association_Status
   
END ; FUNCTION Associated_plot_holder
