;+
; NAME:
;       OSRF::Shift
;
; PURPOSE:
;       The OSRF::Shift procedure method shifts the response data
;       to different frequencies.
;
; CALLING SEQUENCE:
;       Obj->[OSRF::]Shift, dFrequency, Debug=Debug
;
; INPUTS:
;       dFrequency:  Array of frequency shifts which is to be applied to
;                    the SRF passbands.
;                    The number of elements must be the same as the number
;                    of SRF passbands.
;                    UNITS:      N/A
;                    TYPE:       FLOAT
;                    DIMENSION:  Rank-1
;                    ATTRIBUTES: INTENT(IN)
;
; INPUT KEYWORDS:
;       Debug:       Set this keyword for debugging. If set then:
;                    - the error handler for this function is disabled
;                      so that execution halts where the error occurs,
;                    - more verbose output is produced.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
;
; INCLUDE FILES:
;       osrf_parameters: Include file containing OSRF specific
;                        parameter value definitions.
;
;       osrf_pro_err_handler: Error handler code for OSRF procedures.
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 24-Jun-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO OSRF::Shift, $
  dFrequency, $
  Debug=Debug  ; Input keyword

  ; Set up
  COMPILE_OPT HIDDEN
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler
  ; ...Do nothing if unallocated
  IF ( ~ self.Associated(Debug=Debug) ) THEN RETURN
  ; ...Check the input
  self.Get_Property, n_Bands=n_bands, Debug=Debug
  IF ( N_ELEMENTS(dFrequency) NE n_bands ) THEN $
    MESSAGE, "Input frequency shift argument has different number of bands from oSRF!", $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Perform the shift on all bands
  FOR i = 0, n_bands-1 DO BEGIN
    band = i+1
    
    ; Get the band frequency data
    self.Get_Property, band, Debug=Debug, Frequency=f
    
    ; Shift it
    f = f + dFrequency[i]
    
    ; Replace the band frequency data
    self.Set_Property, band, Debug=Debug, Frequency=f
  ENDFOR


  ; Recompute the oSRF parameters
  MESSAGE, "Recomputing oSRF parameters after frequency shift...", /INFORMATIONAL
  self.Integrate, Debug=Debug
  self.Compute_Central_Frequency, Debug=Debug
  self.Compute_Planck_Coefficients, Debug=Debug
  self.Compute_Polychromatic_Coefficients, Debug=Debug

END
