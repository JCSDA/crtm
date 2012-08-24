;+
; NAME:
;       OSRF::Convolve_Radiance
;
; PURPOSE:
;       The OSRF::Convolve_Radiance procedure method convolves the "Radiance"
;       component data with the response data.
;
; CALLING SEQUENCE:
;       Obj->[OSRF::]Convolve_Radiance, Debug = Debug
;
; INPUT KEYWORD PARAMETERS:
;       Debug:                 Set this keyword for debugging.
;                              If NOT SET => Error handler is enabled. (DEFAULT)
;                                 SET     => Error handler is disabled; Routine
;                                            traceback output is enabled.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
; INCLUDE FILES:
;       osrf_parameters: Include file containing OSRF specific
;                        parameter value definitions.
;
;       osrf_pro_err_handler: Error handler code for OSRF procedures.
;
; EXAMPLE:
;       Given a valid OSRF object, x, the Planck radiance is computed,
;
;         IDL> x->Compute_Planck_Radiance(300.0d0)
;
;       The computed radiances are then convolved with the response,
;
;         IDL> x->Convolve_Radiance
;
;       The resultant convolved radiances can then be obtained via
;       the Get_Property method:
;
;         IDL> x->Get_Property, Convolved_R = r
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 12-Aug-2010
;                       paul.vandelst@noaa.gov
;
;-

PRO OSRF::Convolve_Radiance, $
  Debug=Debug

  ; Set up
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler


  ; Check if object has been allocated
  IF ( ~ self->Associated(Debug=Debug) ) THEN $
    MESSAGE, 'OSRF object has not been allocated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Perform the convolution
  self.Convolved_R = self->Convolve( *self.Radiance, Debug=Debug )

END ; PRO OSRF::Convolve_Radiance
