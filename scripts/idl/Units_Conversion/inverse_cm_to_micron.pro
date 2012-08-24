;+
;
; NAME:
;       inverse_cm_to_micron
;
; PURPOSE:
;       Function to convert frequency in inverse centimetres to
;       wavelengths in microns.
;
; CALLING SEQUENCE:
;       wavelength = inverse_cm_to_micron( frequency )
;
; INPUTS:
;       frequency:   Frequency to convert to wavelength.
;                    Must be > 0.0.
;                    UNITS:      inverse centimetres (cm^-1)
;                    ATTRIBUTES: INTENT(IN)
;
; FUNCTION RESULT:
;       wavelength:  FRequency converted to wavelength.
;                    Set to -ve value if input is invalid.
;                    UNITS:      microns (um)
;                    TYPE:       DOUBLE
;
; PROCEDURE:
;       The relationship between wavelength and frequency is given by the
;       simple units conversion formula,
;
;                                10000
;         wavelength(um) = ------------------
;                           frequency(cm^-1)
;
;       where the factor of 10000 is because 1 um == 10^-4 cm.
;
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 28-Jun-2010
;                       paul.vandelst@noaa.gov
;
;-

FUNCTION inverse_cm_to_micron, frequency

  ; Include files
  @fundamental_constants
  
  ; Parameters
  INVERSECM_TO_UM = 1.0d+04
  
  ; Set up error handler
  @error_codes
  CATCH, error_status
  IF ( error_status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN, -1
  ENDIF    

  ; Check input
  IF ( N_ELEMENTS( frequency ) EQ 0 ) THEN $
    MESSAGE, 'Input FREQUENCY argument not defined!', $
             /NONAME, /NOPRINT
  ;...Always use double precision
  f = DOUBLE( frequency )
  index = WHERE( f LT ZERO, count )
  IF ( count GT 0 ) THEN $
    MESSAGE, 'Negative frequency input. Units must be inverse centimetres.', $
             /NONAME, /NOPRINT

  ; Convert from cm-1 to microns
  w = INVERSECM_TO_UM / f

  ; Done
  CATCH, /CANCEL
  RETURN, w

END
