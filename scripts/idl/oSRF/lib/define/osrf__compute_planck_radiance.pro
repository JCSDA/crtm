;+
; NAME:
;       OSRF::Compute_Planck_Radiance
;
; PURPOSE:
;       The OSRF::Compute_Planck_Radiance procedure method computes the
;       monochromatic Planck radiances at the frequencies specified by
;       the OSRF data. The computed radiances are stored in the internal
;       "Radiance" component of the OSRF object.
;
; CALLING SEQUENCE:
;       Obj->[OSRF::]Compute_Planck_Radiance, Temperature, Debug = Debug
;
; INPUTS:
;       Temperature:           The temperature at which the Planck
;                              radiances are required.
;                              UNITS:      Kelvin
;                              TYPE:       DOUBLE
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN)
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
;       Given a valid OSRF object, x, the Planck radiances for a temperature
;       of 300K would be computed like so,
;
;         IDL> x->Compute_Planck_Radiance, 300.0d0
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 17-Jun-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO OSRF::Compute_Planck_Radiance, $
  Temperature, $
  Debug=Debug

  ; Set up
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler


  ; Check if object has been allocated
  IF ( ~self.Associated(Debug=Debug) ) THEN $
    MESSAGE, 'OSRF object has not been allocated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Compute Planck radiance for each band
  FOR i = 0L, self.n_Bands-1L DO BEGIN
    Band = i+1
    self.Get_Property, $
      Band, $
      Frequency=f, $
      Debug=Debug
    IF ( self.Flag_Is_Set(FREQUENCY_GHZ_FLAG) ) THEN f = GHz_to_inverse_cm(f)
    result = Planck_Radiance(f, Temperature, radiance)
    IF ( result NE SUCCESS ) THEN $
      MESSAGE, 'Error computing Planck radiance for band '+STRTRIM(i+1,2), $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    self.Set_Property, $
      Band, $
      Radiance=radiance, $
      Debug=Debug
  ENDFOR

END
