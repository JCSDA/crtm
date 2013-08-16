;+
; NAME:
;       OSRF::Reflect
;
; PURPOSE:
;       The OSRF::Reflect procedure method reflects the high frequency
;       bands of a multiband SRF into the lower frequency bands.
;
; CALLING SEQUENCE:
;       Obj->[OSRF::]Reflect, Debug=Debug
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

PRO OSRF::Reflect, $
  Debug=Debug  ; Input keyword

  ; Set up
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler
 
 
  ; Check object
  ; ...Do nothing if unallocated
  IF ( ~self.Associated(Debug=Debug) ) THEN RETURN
  ; ...Check this SRF can be reflected
  self.Get_Property, n_Bands=n_bands, Debug=Debug
  IF ( n_bands LT 2 ) THEN RETURN
  IF ( (n_bands MOD 2) NE 0 ) THEN BEGIN
    MESSAGE, "Number of OSRF bands is odd! No reflection performed.", /INFORMATIONAL
    RETURN
  ENDIF


  ; Get the central frequency
  self.Get_Property, Debug=Debug, f0=f0
  MESSAGE, "Retrieved central frequency of "+STRING(f0,FORMAT='(e13.6)')+$
           " for band reflection.", /INFORMATIONAL
  
 
  ; Generate the array index positions
  n_halfbands = n_bands/2L
  hidx = LINDGEN(n_halfbands)
  oidx = hidx + n_halfbands  ; The indices of the data to reflect
  ridx = REVERSE(hidx)       ; The indices into which the reflected data is inserted
  
  
  ; Perform the reflections
  FOR i = 0, n_halfbands-1 DO BEGIN
  
    ; Get the original band data
    oband = oidx[i]+1
    self.Get_Property, oband, Debug=Debug, Frequency=f, Response=r
    
    ; Reverse the data
    f = TWO*f0 - REVERSE(f)
    r = REVERSE(r)
    
    ; Set the reflected band data
    rband = ridx[i]+1
    self.Set_Property, rband, Debug=Debug, Frequency=f, Response=r
    
  ENDFOR


  ; Recompute the oSRF parameters
  MESSAGE, "Recomputing oSRF parameters after passband reflection...", /INFORMATIONAL
  self.Integrate, Debug=Debug
  self.Compute_Central_Frequency, Debug=Debug
  self.Compute_Planck_Coefficients, Debug=Debug
  self.Compute_Polychromatic_Coefficients, Debug=Debug
  
END
