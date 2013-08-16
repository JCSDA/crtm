;+
; NAME:
;       OSRF::Convolved_R2T
;
; PURPOSE:
;       The OSRF::Convolved_R2T converts the convolved radiance to a
;       brightness temperature.
;
; CALLING SEQUENCE:
;       Obj->[OSRF::]Convolved_R2T, $
;              No_Band_Correction = No_Band_Correction, $
;              Debug = Debug
;
; INPUT KEYWORD PARAMETERS:
;       No_Band_Correction:    Set this keyword to NOT apply the polychromatic
;                              band correction to the computed temperature.
;                              If NOT SET => Correction is applied. (DEFAULT)
;                                 SET     => Correction is NOT applied.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
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
;       The convolved radiance is then converted to a temperature,
;
;         IDL> x->Convolved_R2T
;
;       The resultant channel brightness temperature value can then be
;       obtained via the Get_Property method:
;
;         IDL> x->Get_Property, Convolved_T = t
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 12-Aug-2010
;                       paul.vandelst@noaa.gov
;
;-

PRO OSRF::Convolved_R2T, $
  No_Band_Correction = No_Band_Correction, $
  Debug=Debug

  ; Set up
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler
  ; ...Process keywords
  apply_band_correction = ~KEYWORD_SET(No_Band_Correction)


  ; Check if object has been allocated
  IF ( ~self.Associated(Debug=Debug) ) THEN $
    MESSAGE, 'OSRF object has not been allocated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Compute the central frequency if necessary
  IF ( ~self.Flag_Is_Set(F0_COMPUTED_FLAG) ) THEN self.Compute_Central_Frequency, Debug=Debug

  
  ; Get the required object properties
  self.Get_Property, $
    f0                   = f0, $
    Polychromatic_Coeffs = pc, $
    Convolved_R          = R , $
    Debug=Debug
  IF ( self.Flag_Is_Set(FREQUENCY_GHZ_FLAG) ) THEN f0 = GHz_to_inverse_cm(f0)


  ; Compute the effective temperature from the channel radiance  
  result = Planck_Temperature( f0, R, Teff )
  IF ( result NE SUCCESS ) THEN $
    MESSAGE, 'Error computing Planck temperature', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Apply the band correction if required
  IF ( apply_band_correction ) THEN BEGIN
    T = (Teff - pc[0])/pc[1]
  ENDIF ELSE BEGIN
    T = Teff
  ENDELSE

  
  ; Save the result
  self.Set_Property, Convolved_T=T, Debug=Debug

END
