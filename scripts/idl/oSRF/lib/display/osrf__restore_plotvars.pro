;+
; NAME:
;       OSRF::Restore_PlotVars
;
; PURPOSE:
;       The OSRF::Restore_PlotVars procedure method restores IDL plotting
;       variables.
;
;       NOTE: This method should be considered PRIVATE to the class
;             and should not be invoked outside OSRF methods.
;
;
; CALLING SEQUENCE:
;       Obj->[OSRF::]Restore_PlotVars, $
;         Window_Reference, $
;         Plot_Reference, $
;         Debug = Debug ; Input keyword
;
; OUTPUT ARGUMENTS:
;       Window_Reference:  The graphics window object reference for the plot.
;                          UNITS:      N/A
;                          TYPE:       GRAPHICSWIN object
;                          DIMENSION:  Scalar
;                          ATTRIBUTES: INTENT(OUT)
;
;       Plot_Reference:    The graphics plot object references for the passbands.
;                          UNITS:      N/A
;                          TYPE:       PLOT object
;                          DIMENSION:  Rank-1 (n_Passbands)
;                          ATTRIBUTES: INTENT(OUT)
;
; KEYWORDS:
;       Debug:             Set this keyword for debugging.
;                          If NOT SET => Error handler is enabled. (DEFAULT)
;                             SET     => Error handler is disabled; Routine
;                                        traceback output is enabled.
;                          UNITS:      N/A
;                          TYPE:       INTEGER
;                          DIMENSION:  Scalar
;                          ATTRIBUTES: INTENT(IN), OPTIONAL
;
; INCLUDE FILES:
;       osrf_parameters: Include file containing OSRF specific
;                        parameter value definitions.
;
;       osrf_pro_err_handler: Error handler code for OSRF procedures.
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 20-Apr-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO OSRF::Restore_PlotVars, $
  Window_Reference, $  ; Output
  Plot_Reference  , $  ; Output
  Debug=Debug          ; Input keyword

  ; Set up
  COMPILE_OPT HIDDEN
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler

  ; ...ALL *input* pointers must be associated
  IF ( NOT self->Associated(Debug=Debug) ) THEN $
    MESSAGE, 'Some or all input OSRF pointer members are NOT associated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch

  Window_Reference = *self.window_reference
  Plot_Reference   = *self.plot_reference

END ; PRO OSRF::Restore_PlotVars
