;+
; NAME:
;       OSRF::Save_PlotVars
;
; PURPOSE:
;       The OSRF::Save_PlotVars procedure method saves IDL plotting
;       system variables.
;
;       NOTE: This method should be considered PRIVATE to the class
;             and should not be invoked outside OSRF methods.
;
;
; CALLING SEQUENCE:
;       Obj->[OSRF::]Save_PlotVars, $
;         Band_Index, $
;         Debug = Debug ; Input keyword
;
; INPUT ARGUMENTS:
;       Band_Index:  The OSRF band for which the plotting system variables
;                    are to be saved.
;                    UNITS:      N/A
;                    TYPE:       OSRF object
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN)
;
; KEYWORDS:
;       Debug:       Set this keyword for debugging.
;                    If NOT SET => Error handler is enabled. (DEFAULT)
;                       SET     => Error handler is disabled; Routine
;                                  traceback output is enabled.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
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
  Band_Index, $
  Debug=Debug ; Input keyword

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

  (*self.psysvar)[Band_Index] = !P
  (*self.xsysvar)[Band_Index] = !X
  (*self.ysysvar)[Band_Index] = !Y

END ; PRO OSRF::Save_PlotVars
