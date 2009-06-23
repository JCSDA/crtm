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
    OSRF_INTERPOLATED_FLAG_STATUS[self->Flag_Is_Set(INTERPOLATED_FLAG)]
  PRINT, 'Integration status   : ', $
    OSRF_INTEGRATED_FLAG_STATUS[self->Flag_Is_Set(INTEGRATED_FLAG)]
  PRINT, 'f0 computation status: ', $
    OSRF_F0_COMPUTED_FLAG_STATUS[self->Flag_Is_Set(F0_COMPUTED_FLAG)]
  PRINT, 'Frequency units      : ', $
    OSRF_FREQUENCY_UNITS_FLAG_STATUS[self->Flag_Is_Set(FREQUENCY_UNITS_FLAG)]
  PRINT, 'Interpolation method : ', $
    OSRF_INTERPOLATION_METHOD_FLAG_STATUS[self->Flag_Is_Set(INTERPOLATION_METHOD_FLAG)]
  PRINT, 'Integration method   : ', $
    OSRF_INTEGRATION_METHOD_FLAG_STATUS[self->Flag_Is_Set(INTEGRATION_METHOD_FLAG)]

  
  ; Done
  CATCH, /CANCEL
 
END ; PRO OSRF::Display_Flags
