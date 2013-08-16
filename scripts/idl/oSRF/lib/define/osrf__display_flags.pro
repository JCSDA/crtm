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
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler
 
  
  ; Display flags
  PRINT, 'Interpolation status : ', $
    IS_INTERPOLATED_FLAG.status[self.Flag_Is_Set(IS_INTERPOLATED_FLAG)]
  PRINT, 'Integration status   : ', $
    IS_INTEGRATED_FLAG.status[self.Flag_Is_Set(IS_INTEGRATED_FLAG)]
  PRINT, 'f0 computation status: ', $
    F0_COMPUTED_FLAG.status[self.Flag_Is_Set(F0_COMPUTED_FLAG)]
  PRINT, 'Frequency units      : ', $
    FREQUENCY_GHZ_FLAG.status[self.Flag_Is_Set(FREQUENCY_GHZ_FLAG)]
  PRINT, 'Interpolation method : ', $
    LINEAR_INTERPOLATION_FLAG.status[self.Flag_Is_Set(LINEAR_INTERPOLATION_FLAG)]

END
