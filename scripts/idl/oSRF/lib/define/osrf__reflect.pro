;+
; NAME:
;       OSRF::Reflect
;
; PURPOSE:
;       The OSRF::Reflect procedure method reflects the high frequency
;       bands of a multiband SRF into the lower frequency bands.
;
; CALLING SEQUENCE:
;       Obj->[OSRF::]Reflect, $
;         Debug=Debug         ; Input keyword
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
  IF ( NOT self->Associated(Debug=Debug) ) THEN RETURN
  IF ( self.n_Bands LT 2 ) THEN RETURN
  IF ( (self.n_Bands MOD 2) NE 0 ) THEN BEGIN
    MESSAGE, 'Number of OSRF bands is odd! No reflection performed', /INFORMATIONAL
    RETURN
  ENDIF
  IF ( self->Flag_Is_Set(FREQUENCY_SHIFT_FLAG) ) THEN BEGIN
    MESSAGE, 'OSRF data has been frequency shifted! No reflection performed', /INFORMATIONAL
    RETURN
  ENDIF
 
 
  ; Generate the array index positions
  n_HalfBands = self.n_Bands/2L
  hIdx = LINDGEN(n_HalfBands)
  oIdx = hIdx + n_HalfBands  ; The indices of the data to reflect
  rIdx = REVERSE(hIdx)       ; The indices into which the reflected data is inserted
  
  
  ; Perform the reflections
  FOR i = 0, n_HalfBands-1 DO BEGIN
    ; Get the original band data
    oBand = oIdx[i]+1
    self->Get_Property, $
      oBand, $
      Debug = Debug, $
      Frequency = f, $
      Response = r
    ; Reverse the data
    f = -ONE * REVERSE(f)
    r = REVERSE(r)
    ; Set the reflected band data
    rBand = rIdx[i]+1
    self->Set_Property, $
      rBand, $
      Debug = Debug, $
      Frequency = f, $
      Response = r
    ; Set the frequency shift values also
    (*self.delta_f)[rIdx[i]] = (*self.delta_f)[oIdx[i]]
  ENDFOR

 
  ; Set the reflected flag
  self->Set_Flag, /Band_Reflect

END ; PRO OSRF::Reflect
