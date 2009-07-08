;+
; NAME:
;       AtmProfile::Restore_PlotVars
;
; PURPOSE:
;       The AtmProfile::Restore_PlotVars procedure method restores IDL plotting
;       system variables.
;
;       NOTE: This method should be considered PRIVATE to the class
;             and should not be invoked outside AtmProfile methods.
;
;
; CALLING SEQUENCE:
;       Obj->[AtmProfile::]Restore_PlotVars, $
;         Band_Index, $
;         Debug = Debug ; Input keyword
;
; INPUT ARGUMENTS:
;       Band_Index:  The AtmProfile band for which the plotting system variables
;                    are to be restored.
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
;       Written by:     Paul van Delst, 20-Apr-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO AtmProfile::Restore_PlotVars, $
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

  !P = (*self.psysvar)[Index]
  !X = (*self.xsysvar)[Index]
  !Y = (*self.ysysvar)[Index]

END ; PRO AtmProfile::Restore_PlotVars
