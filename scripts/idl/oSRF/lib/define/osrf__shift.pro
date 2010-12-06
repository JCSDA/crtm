;+
; NAME:
;       OSRF::Shift
;
; PURPOSE:
;       The OSRF::Shift procedure method shifts the response data
;       to different frequencies.
;
; CALLING SEQUENCE:
;       Obj->[OSRF::]Shift, $
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

PRO OSRF::Shift, $
  Debug=Debug  ; Input keyword

  ; Set up
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler
 
 
  ; Check object
  IF ( NOT self->Associated(Debug=Debug) ) THEN RETURN
 

  ; Perform the shift on all bands
  FOR i = 0, self.n_Bands-1 DO BEGIN
    Band = i+1
    ; Get the band frequency data
    self->Get_Property, $
      Band, $
      Debug = Debug, $
      Frequency = f
    ; Shift it
    f = f + (*self.delta_f)[i]
    ; Replace the band frequency data
    self->Set_Property, $
      Band, $
      Debug = Debug, $
      Frequency = f
  ENDFOR
  
  
  ; Set the shift flag
  self->Set_Flag, /Frequency_Shift
    
END ; PRO OSRF::Shift
