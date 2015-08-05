;+
; NAME:
;       OSRF::Display_Flags
;
; PURPOSE:
;       The OSRF::Display_Flags procedure displays the current settings
;       of the OSRF bit flag component.
;
; CALLING SEQUENCE:
;       Obj->[OSRF::]Display_Flags, $
;         Debug = Debug ; Input keyword
;         
; INPUT KEYWORD PARAMETERS:
;       Debug:  Set this keyword for debugging.
;               If NOT SET => Error handler is enabled. (DEFAULT)
;                  SET     => Error handler is disabled; Routine
;                             traceback output is enabled.
;               UNITS:      N/A
;               TYPE:       INTEGER
;               DIMENSION:  Scalar
;               ATTRIBUTES: INTENT(IN), OPTIONAL
;
; INCLUDE FILES:
;       osrf_parameters: Include file containing OSRF specific
;                        parameter value definitions.
;
;       osrf_pro_err_handler: Error handler code for OSRF functions.
;
; EXAMPLE:
;       Given a valid, allocated, OSRF object, x, the various bit flags settings
;       can be displayed like so,
;
;         IDL> x->Display_Flags
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 15-Jun-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO OSRF::Display_Flags, $
  Debug = Debug  ; Input keyword


  ; Set up
  COMPILE_OPT HIDDEN
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler
 
  
  ; Display flags
  PRINT, IS_INTERPOLATED_FLAG.name, ' status: ', $
    IS_INTERPOLATED_FLAG.status[self.Flag_Is_Set(IS_INTERPOLATED_FLAG)]
  PRINT, IS_INTEGRATED_FLAG.name, ' status: ', $
    IS_INTEGRATED_FLAG.status[self.Flag_Is_Set(IS_INTEGRATED_FLAG)]
  PRINT, F0_COMPUTED_FLAG.name, ' status: ', $
    F0_COMPUTED_FLAG.status[self.Flag_Is_Set(F0_COMPUTED_FLAG)]
  PRINT, LINEAR_INTERPOLATION_FLAG.name, ' status: ', $
    LINEAR_INTERPOLATION_FLAG.status[self.Flag_Is_Set(LINEAR_INTERPOLATION_FLAG)]
  PRINT, IS_DIFFERENCE_FLAG.name, ' status: ', $
    IS_DIFFERENCE_FLAG.status[self.Flag_Is_Set(IS_DIFFERENCE_FLAG)]
  PRINT, IS_HIRES_FLAG.name, ' status: ', $
    IS_HIRES_FLAG.status[self.Flag_Is_Set(IS_HIRES_FLAG)]

END
