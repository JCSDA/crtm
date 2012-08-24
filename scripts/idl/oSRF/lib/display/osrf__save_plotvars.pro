;+
; NAME:
;       OSRF::Save_PlotVars
;
; PURPOSE:
;       The OSRF::Save_PlotVars procedure method saves IDL plotting
;       variables.
;
;       NOTE: This method should be considered PRIVATE to the class
;             and should not be invoked outside OSRF methods.
;
;
; CALLING SEQUENCE:
;       Obj->[OSRF::]Save_PlotVars, $
;         Window_Reference, $
;         Plot_Reference, $
;         Debug = Debug ; Input keyword
;
; INPUT ARGUMENTS:
;       Window_Reference:  The graphics window object reference for the plot.
;                          UNITS:      N/A
;                          TYPE:       GRAPHICSWIN object
;                          DIMENSION:  Scalar
;                          ATTRIBUTES: INTENT(IN)
;
;       Plot_Reference:    The graphics plot object references for the passbands.
;                          UNITS:      N/A
;                          TYPE:       PLOT object
;                          DIMENSION:  Rank-1 (n_Passbands)
;                          ATTRIBUTES: INTENT(IN)
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

PRO OSRF::Save_PlotVars, $
  Window_Reference, $  ; Input
  Plot_Reference  , $  ; Input
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

  self.window_reference = PTR_NEW(Window_Reference)
  self.plot_reference   = PTR_NEW(Plot_Reference)

END ; PRO OSRF::Save_PlotVars
