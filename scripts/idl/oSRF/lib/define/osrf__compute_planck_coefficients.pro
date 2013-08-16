;+
; NAME:
;       OSRF::Compute_Planck_Coefficients
;
; PURPOSE:
;       The OSRF::Compute_Planck_Coefficients procedure method computes the
;       Planck function coefficients for the OSRF channel.
;
; CALLING SEQUENCE:
;       Obj->[OSRF::]Compute_Planck_Coefficients, Debug = Debug
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
;       Given a valid OSRF object, x, the Planck coefficients are computed like so,
;
;         IDL> x->Compute_Planck_Coefficients
;
;       The coefficients can be obtained via the Get_Property method:
;
;         IDL> x->Get_Property, Planck_Coeffs = pc
;         IDL> HELP, pc
;         PC      DOUBLE    = Array[2]
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 17-Jun-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO OSRF::Compute_Planck_Coefficients, $
  Debug=Debug

  ; Set up
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler
  ; ...Definition of C1 and C2
  @fundamental_constants
  ; ...Parameters for C1, C2 scaling
  ; The following scaling factors are applied to produce radiances in units
  ; of mW/(m^2.sr.cm^-1) when they are used.
  ;
  ; First Planck function constant (C1) scale factors. Units of C1 are W.m^2.
  ; Length scaling: To convert to W/(m^2.cm^-4) requires a scaling of m->cm,
  ;                 which is 100, to the fourth power, which is 1.0e+08.
  ; Power scaling:  To convert to mW.m^2 requires a scaling of 1000.
  C1_LENGTH_SCALE_FACTOR = 1.0d+08
  C1_POWER_SCALE_FACTOR  = 1.0d+03
  C1_SCALE_FACTOR = C1_LENGTH_SCALE_FACTOR * C1_POWER_SCALE_FACTOR
  ; Second Planck function constant (C2) scale factor. Units of C2 are K.m,
  ; So to convert to K.cm, a scaling of 100 is applied.
  C2_SCALE_FACTOR = 100.0d0
  
  
  ; Check if object has been allocated
  IF ( ~self.Associated(Debug=Debug) ) THEN $
    MESSAGE, 'OSRF object has not been allocated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Compute the central frequency if necessary
  IF ( ~self.Flag_Is_Set(F0_COMPUTED_FLAG) ) THEN self.Compute_Central_Frequency, Debug=Debug
  

  ; Check frequency units
  self.Get_Property, f0=f0, Debug=Debug
  IF ( self.Flag_Is_Set(FREQUENCY_GHZ_FLAG) ) THEN f0 = GHz_to_inverse_cm(f0)


  ; Compute the Planck coefficients
  planck_coeffs = DBLARR(N_PLANCK_COEFFS)
  planck_coeffs[0] = C1_SCALE_FACTOR * C1 * f0^3
  planck_coeffs[1] = C2_SCALE_FACTOR * C2 * f0
  self.Planck_Coeffs = planck_coeffs

END
