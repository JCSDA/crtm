;+
; NAME:
;       oSRF::Zero_Negative
;
; PURPOSE:
;       The OSRF::Zero_Negative procedure sets any negative response
;       values to zero. If any negative values are found and zeroed,
;       the SRF parameters (integrated area, central frequency, Planck
;       and polychromatic coefficients) are recalculated.
;
; CALLING SEQUENCE:
;       Obj->[oSRF::]Zero_Negative, $
;         No_Recalculate = No_Recalculate, $  ; Input keyword
;         Debug          = Debug              ; Input keyword
;
; INPUT KEYWORDS:
;       No_Recalculate: Set this keyword to prevent the SRF parameters,
;                       from being recalculated. These parameters are
;                       recomputed by default if negative response
;                       values are found.
;                       UNITS:      N/A
;                       TYPE:       INTEGER
;                       DIMENSION:  Scalar
;                       ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Debug:          Set this keyword for debugging. If set then:
;                       - the error handler for this function is disabled
;                         so that execution halts where the error occurs,
;                       - more verbose output is produced.
;                       UNITS:      N/A
;                       TYPE:       INTEGER
;                       DIMENSION:  Scalar
;                       ATTRIBUTES: INTENT(IN), OPTIONAL
;
; INCLUDE FILES:
;       osrf_parameters: Include file containing OSRF specific
;                        parameter value definitions.
;
;       osrf_pro_err_handler: Error handler code for OSRF procedures.
;
; EXAMPLE:
;       Given a valid oSRF object, any ngative values can be zeroed
;       like so
;
;         IDL> osrf->Zero_Negative, Debug=Debug
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 09-Dev-2010
;                       paul.vandelst@noaa.gov
;
;-

PRO OSRF::Zero_Negative, $
  No_Recalculate = No_Recalculate, $
  Debug = Debug  
  
  ; Set up
  ; ...oSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler
  ; ...Check keywords
  recalculate = ~ KEYWORD_SET(No_Recalculate)


  ; Check if object has been allocated
  IF ( ~ self->Associated(Debug=Debug) ) THEN $
    MESSAGE, 'OSRF object has not been allocated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
 
  
  ; Get the number of bands
  self->Get_Property, n_Bands=n_bands, Debug=Debug


  ; Initialise negative value counter
  negative_count = 0L
  
  
  ; Loop over bands
  FOR i = 0, n_bands-1 DO BEGIN
    band = i+1
    
    ; Get the current band response
    self->Get_Property, band, Response=r, Debug=Debug
    
    ; Locate any negative values
    idx = WHERE(r LT ZERO, count)
    IF ( count EQ 0 ) THEN CONTINUE
    ; ...Increment counter
    negative_count = negative_count + count  
    ; ...Zero the values
    r[idx] = ZERO

    ; Replace the band response
    self->Set_Property, band, Response=r, Debug=Debug

  ENDFOR


  ; Recompute the various SRF parameters if necessary
  IF ( recalculate AND (negative_count GT 0) ) THEN BEGIN
    self->Integrate, Debug=Debug
    self->Compute_Central_Frequency, Debug=Debug
    self->Compute_Planck_Coefficients, Debug=Debug
    self->Compute_Polychromatic_Coefficients, Debug=Debug
  ENDIF
  
END

