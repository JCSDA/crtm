;+
; Function to deallocate the pointer
; components of a plot_holder structure

FUNCTION Destroy_plot_holder, plot_holder, $                ; Output
                              No_Clear = No_Clear   ; Input keyword
;-
 
  ; Generic SRF parameters
  @srf_parameters
  
  ; Set up error handler
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN, FAILURE
  ENDIF
 
  ; Check the structure
  IF ( Is_A_plot_holder_Structure( plot_holder ) EQ FALSE ) THEN $
    MESSAGE, 'Input structure is not a plot_holder structure', $
             /NONAME, /NOPRINT
  
  ; Initialise the scalar members
  IF ( NOT KEYWORD_SET(No_Clear) ) THEN BEGIN
    plot_holder.Ifile           = ' '
    plot_holder.Sensor_Id       = ' '
    plot_holder.Sensor_Type     = ' '
    plot_holder.Channel_Name    = ' '
    plot_holder.v1              = ZERO
    plot_holder.v2              = ZERO
    plot_holder.f0              = ZERO
    plot_holder.Percent_Removed = ZERO
    plot_holder.Negative_Count  = 0L
  ENDIF

  ; If ALL pointer members are NOT associated, do nothing
  IF ( Associated_plot_holder(plot_holder) EQ FALSE ) THEN GOTO, Done

  ; Deallocate the pointer members and nullify
  PTR_FREE, plot_holder.f     , $
            plot_holder.r     , $
            plot_holder.orig_f, $
            plot_holder.orig_r, $
            plot_holder.f0    , $
            plot_holder.f0_doc, $
            plot_holder.f0_hm , $
            plot_holder.f0_fm , $
            plot_holder.f_fm  , $
            plot_holder.f_hm  , $
            plot_holder.f_doc  
                       
  plot_holder.f      = PTR_NEW()
  plot_holder.r      = PTR_NEW()
  plot_holder.orig_f = PTR_NEW()
  plot_holder.orig_r = PTR_NEW()
  plot_holder.f0     = PTR_NEW() 
  plot_holder.f0_doc = PTR_NEW()
  plot_holder.f0_hm  = PTR_NEW()
  plot_holder.f0_fm  = PTR_NEW()
  plot_holder.f_fm   = PTR_NEW()
  plot_holder.f_hm   = PTR_NEW()
  plot_holder.f_doc  = PTR_NEW()
  
  ; Decrement and test allocation counter
  plot_holder.n_Allocates = plot_holder.n_Allocates - 1
  IF ( plot_holder.n_Allocates NE 0 ) THEN $
    MESSAGE, 'Allocation counter /= 0, Value = ' + STRTRIM(plot_holder.n_Allocates, 2), $
             /NONAME, /NOPRINT

  ; Done
  Done:
  CATCH, /CANCEL
  RETURN, SUCCESS

END ; FUNCTION Destroy_plot_holder





