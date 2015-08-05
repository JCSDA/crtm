;+
; NAME:
;       OSRF::Interpolate
;
; PURPOSE:
;       The OSRF::Interpolate procedure method interpolates the response data
;       to the frequency grid loaded in the interpolation oSRF object.
;
; CALLING SEQUENCE:
;       Obj->[OSRF::]Interpolate, $
;         int_oSRF     , $
;         Debug = Debug, $
;         Sigma = Sigma
;
; OUTPUTS:
;
; INPUT KEYWORD PARAMETERS:
;       Debug:    Set this keyword for debugging.
;                 If NOT SET => Error handler is enabled. (DEFAULT)
;                    SET     => Error handler is disabled; Routine
;                               traceback output is enabled.
;                 UNITS:      N/A
;                 TYPE:       INTEGER
;                 DIMENSION:  Scalar
;                 ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Sigma:    If non-linear interpolation is specified, this is the
;                 amount of tension that is applied to the spline. The
;                 default oSRF value is 5.0. If sigma is close to 0, (e.g., .01),
;                 then effectively there is a cubic spline fit. If sigma
;                 is large, (e.g., greater than 10), then the fit will be
;                 like a polynomial interpolation.
;                 UNITS:      N/A
;                 TYPE:       FLOAT
;                 DIMENSION:  Scalar
;                 ATTRIBUTES: INTENT(IN), OPTIONAL
;
; INCLUDE FILES:
;       osrf_parameters: Include file containing OSRF specific
;                        parameter value definitions.
;
;       osrf_pro_err_handler: Error handler code for OSRF procedures.
;
; EXAMPLE:
;       Given a valid OSRF object, x, and a prepared interpolated oSRF object, x_int,
;       the SRF data is interpolated like so,
;
;         IDL> x->Interpolate, x_int
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 17-Jun-2009
;                       paul.vandelst@noaa.gov
;
;-
PRO OSRF::Interpolate, $
  int_oSRF, $
  Sigma = Sigma, $
  Debug = Debug

  ; Set up
  COMPILE_OPT HIDDEN
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler
 

  ; Check if object has been allocated
  IF ( ~ self.Associated(Debug=Debug) ) THEN $
    MESSAGE, 'oSRF object has not been allocated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Check that the object argument is an allocated oSRF object
  IF ( ~ OBJ_ISA(int_oSRF,'oSRF') ) THEN $
    MESSAGE, 'Output object class is not oSRF.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  IF ( ~ int_oSRF.Associated(Debug=Debug) ) THEN $
    MESSAGE, 'Output oSRF object has not been allocated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Process the sigma keyword
  _sigma = N_ELEMENTS(Sigma) GT 0 ? DOUBLE(ABS(Sigma[0])) : 5.0d0


  ; Get the number of bands
  self.Get_Property, n_Bands=n_bands, Debug=Debug
  ; ...and check it
  int_oSRF.Get_Property, n_Bands=int_n_bands, Debug=Debug
  IF ( n_bands NE int_n_bands ) THEN $
    MESSAGE, 'Number of bands is different in oSRF objects.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Perform the interpolation
  FOR band = 1L, n_bands DO BEGIN

    ; Get band data
    self.Get_Property, band, Frequency=f, Response=r, Debug=Debug
    int_oSRF.Get_Property, band, Frequency=f_int, Debug=Debug

    ; Perform linear interpolation everywhere first (it's cheap!)
    r_int = INTERPOL( r, f, f_int )
    
    ; Now perform spline interpolation if necessary.
    IF ( ~ self.Flag_Is_Set(LINEAR_INTERPOLATION_FLAG) ) THEN BEGIN
      r_tmp = SPLINE( f, r, f_int, _sigma, /DOUBLE )
      ; Only keep spline data above cutoff to minimise interpolation
      ; artifacts due to noise at low levels in SRF responses
      r_max = MAX(r_tmp)
      r_cutoff = LINEAR_INTERPOLATION_CUTOFF_FRACTION * r_max
      idx = WHERE( r_tmp GE r_cutoff, count )
      IF ( count GT 0 ) THEN r_int[idx] = r_tmp[idx]
    ENDIF
    
    ; Save the result
    int_oSRF.Set_Property, band, Response=r_int, Debug=Debug
    
  ENDFOR
  
  
  ; Recompute the various SRF parameters
  int_oSRF->Integrate, Debug=Debug
  int_oSRF->Compute_Central_Frequency, Debug=Debug
  int_oSRF->Compute_Planck_Coefficients, Debug=Debug
  int_oSRF->Compute_Polychromatic_Coefficients, Debug=Debug
  
END
