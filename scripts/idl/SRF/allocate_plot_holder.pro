; Function to allocate plot holder

FUNCTION Allocate_plot_holder, n_int_points   , $ ;Input
                               n_ori_points   , $ ;Input
                               plot_holder        ;Output



  ; Generic SRF parameters
  @srf_parameters

  ; Set up error handler
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !Error_STATE.MSG, /CONTINUE
    RETURN, FAILURE
  ENDIF

  ; Check dimension input
  IF ( n_int_points LT 1 ) THEN $
    MESSAGE, 'Input N_INT_POINTS must be > 0.', $
             /NONAME, /NOPRINT
  
  ; Check dimension input
  IF ( n_ori_points LT 1 ) THEN $
    MESSAGE, 'Input N_ORI_POINTS must be > 0.', $
             /NONAME, /NOPRINT

  ; Check the structure
  IF ( Is_A_plot_holder_Structure( plot_holder, /Quiet ) EQ TRUE ) THEN BEGIN

    ; Check if ANY pointers are already associated
    ; If they are, deallocate them but leave scalars.
    IF ( Associated_plot_holder( plot_holder, /ANY_Test ) EQ TRUE ) THEN BEGIN
      Result = Destroy_plot_holder( plot_holder, /No_Clear )
      IF ( Result NE SUCCESS ) THEN $
        MESSAGE, 'Error deallocating plot_holder pointer members.', $
                 /NONAME, /NOPRINT
    ENDIF
  ENDIF ELSE BEGIN

    ; plot_holder argument is not a plot_holder structure.
    ; So, make it one.
    plot_holder = {plot_holder}
  ENDELSE

  ; perform the allocations
  plot_holder.f      = PTR_NEW(DBLARR(n_int_points))
  plot_holder.r      = PTR_NEW(DBLARR(n_int_points))
  plot_holder.orig_f = PTR_NEW(DBLARR(n_ori_points))
  plot_holder.orig_r = PTR_NEW(DBLARR(n_ori_points))

  ; Increment and test allocation counter
  plot_holder.n_Allocates = plot_holder.n_Allocates + 1
  IF ( plot_holder.n_Allocates NE 1 ) THEN $
    MESSAGE, 'Allocation counter /= 1, Value = ' + STRTRIM(plot_holder.n_Allocates,2), $
             /NONAME, /NOPRINT
 
  ; Done
  CATCH, /CANCEL
  RETURN, SUCCESS
 
END ; FUNCTION Allocate_plot_holder
  
  

