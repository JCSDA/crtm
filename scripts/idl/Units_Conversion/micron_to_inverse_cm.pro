;+
;
; NAME:
;       micron_to_inverse_cm
;
; PURPOSE:
;       Function to convert wavelengths in microns to frequency in inverse
;       centimetres
;
; CALLING SEQUENCE:
;       frequency = micron_to_inverse_cm( wavelength )
;
; INPUTS:
;       wavelength:  Wavelength to convert to frequency
;                    Must be > 0.0.
;                    UNITS:      microns (um)
;                    ATTRIBUTES: INTENT(IN)
;
; FUNCTION RESULT:
;       frequency:   Wavelength converted to frequency.
;                    Set to -ve value if input is invalid.
;                    UNITS:      inverse centimetres (cm^-1)
;                    TYPE:       DOUBLE
;
; PROCEDURE:
;       The relationship between wavelength and frequency is given by the
;       simple units conversion formula,
;
;                                 10000
;         frequency(cm^-1) = ----------------
;                             wavelength(um)
;
;       where the factor of 10000 is because 1 um == 10^-4 cm.
;
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 28-Jun-2010
;                       paul.vandelst@noaa.gov
;
;-

FUNCTION micron_to_inverse_cm, wavelength

  ; Include files
  @fundamental_constants
  
  ; Parameters
  UM_TO_INVERSECM = 1.0d+04
  
  ; Set up error handler
  @error_codes
  CATCH, error_status
  IF ( error_status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN, -1
  ENDIF    

  ; Check input
  IF ( N_ELEMENTS( wavelength ) EQ 0 ) THEN $
    MESSAGE, 'Input WAVELENGTH argument not defined!', $
             /NONAME, /NOPRINT
  ;...Always use double precision
  w = DOUBLE( wavelength )
  index = WHERE( w LT ZERO, count )
  IF ( count GT 0 ) THEN $
    MESSAGE, 'Negative wavelength input. Units must be microns.', $
             /NONAME, /NOPRINT

  ; Convert from microns to cm-1
  v = UM_TO_INVERSECM / w

  ; Done
  CATCH, /CANCEL
  RETURN, v

END
