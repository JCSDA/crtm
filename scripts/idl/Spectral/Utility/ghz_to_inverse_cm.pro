;+
;
; NAME:
;       ghz_to_inverse_cm
;
; PURPOSE:
;       Function to convert frequencies in units of gigahertz to inverse
;       centimeters
;
; CALLING SEQUENCE:
;       f_Invcm = ghz_to_inverse_cm( f_GHz )
;
; INPUTS:
;       f_GHz:     Frequency to convert to inverse centimetres.
;                  Must be > 0.0.
;                  UNITS:      gigahertz (GHGz)
;                  ATTRIBUTES: INTENT(IN)
;
; FUNCTION RESULT:
;       f_Invcm:   Frequency converted to inverse centimetres.
;                  Set to -ve value if input is invalid.
;                  UNITS:      inverse centimetres (cm^-1)
;                  TYPE:       DOUBLE
;
; INCLUDE FILES:
;       fundamental_constants:  Include file containing various fundamental
;                               physical constants.
;
; PROCEDURE:
;       The relationship between wavelength and frequency is given by,
;
;              c
;         l = ---  m     .....(1)
;              f
;
;       where c = speed of light in m.s^-1
;             f = frequency in Hz (s^-1).
;
;       The conversion of wavelength, l, to frequency, v, in cm^-1, is given by,
;
;                1
;         v = -------  cm^-1     .....(2)
;              100 l
;
;       where the factor of 100 convert l from metres to centimetres.
;       Substituting (2) into (1) gives,
;
;               f
;         v = -------  cm^-1     .....(3)
;              100 c
;
;       If f is expressed as gigahertz, then (3) becomes,
;
;              10^9 f
;         v = -------- cm^-1
;              100  c
;
;                   f
;           = 10^7 --- cm^-1
;                   c
;
;       Therefore the conversion factor from GHz to inverse centimeters is
;       10^7/c where c is in m.s^-1.
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, CIMSS/SSEC, 09-01-2001
;                       paul.vandelst@noaa.gov
;
;-

FUNCTION ghz_to_inverse_cm, frequency

  ; Include files
  @fundamental_constants
  
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
    MESSAGE, 'Negative frequency input. Units must be GHz.', $
             /NONAME, /NOPRINT

  ; Convert from GHz to cm-1
  v = 1.0d+07 * f / SPEED_OF_LIGHT

  ; Done
  CATCH, /CANCEL
  RETURN, v

END
