;+
; NAME:
;       AtmProfile::Save_PlotVars
;
; PURPOSE:
;       The AtmProfile::Save_PlotVars procedure method saves IDL plotting
;       system variables.
;
;       NOTE: This method should be considered PRIVATE to the class
;             and should not be invoked outside AtmProfile methods.
;
;
; CALLING SEQUENCE:
;       Obj->[AtmProfile::]Save_PlotVars, $
;         Band_Index, $
;         Debug = Debug ; Input keyword
;
; INPUT ARGUMENTS:
;       Band_Index:  The AtmProfile band for which the plotting system variables
;                    are to be saved.
;                    UNITS:      N/A
;                    TYPE:       AtmProfile object
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
;       atmprofile_parameters: Include file containing AtmProfile specific
;                              parameter value definitions.
;
;       atmprofile_pro_err_handler: Error handler code for AtmProfile procedures.
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 06-Jul-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO AtmProfile::Save_PlotVars, $
  Index, $
  Debug=Debug ; Input keyword

  ; Set up
  COMPILE_OPT HIDDEN
  ; ...AtmProfile parameters
  @atmprofile_parameters
  ; ...Set up error handler
  @atmprofile_pro_err_handler

  ; ...ALL *input* pointers must be associated
  IF ( NOT self->Associated(Debug=Debug) ) THEN $
    MESSAGE, 'Some or all input AtmProfile pointer members are NOT associated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch

  (*self.psysvar)[Index] = !P
  (*self.xsysvar)[Index] = !X
  (*self.ysysvar)[Index] = !Y

END ; PRO AtmProfile::Save_PlotVars
