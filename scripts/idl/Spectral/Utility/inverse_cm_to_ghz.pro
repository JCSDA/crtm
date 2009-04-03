;+
;
; NAME:
;       inverse_cm_to_ghz
;
; PURPOSE:
;       Function to convert frequencies in units of inverse centimeters to
;       gigahertz
;
; CALLING SEQUENCE:
;       f_GHz = inverse_cm_to_ghz( f_Invcm )
;
; INPUTS:
;       f_Invcm:   Frequency to convert to gigahertz.
;                  Must be > 0.0.
;                  UNITS:      inverse centimetres (cm^-1)
;                  TYPE:       FLOAT or DOUBLE
;                  ATTRIBUTES: INTENT(IN)
;
; FUNCTION RESULT:
;       f_GHz:     Frequency converted to gigahertz.
;                  Set to -ve value if input is invalid.
;                  UNITS:      gigahertz (GHGz)
;                  TYPE:       DOUBLE
;                  ATTRIBUTES: INTENT(IN)
;
; PROCEDURE:
;       The relationship between frequency and wavelength is given by,
;
;              c
;         f = ---  Hz (s^-1)     .....(1)
;              l
;
;       where c = speed of light in m.s^-1
;             l = wavelength in m.
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
;         f = 100 c.v  Hz
;
;           = 10^-9 . 100 c.v  GHz
;
;           = 10^-7 . c.v  GHz
;
;       Therefore the conversion factor from inverse centimeters to GHz is
;       10^-7.c where c is in m.s^-1.
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, CIMSS/SSEC, 09-01-2001
;                       paul.vandelst@noaa.gov
;
;-

FUNCTION inverse_cm_to_ghz, frequency

  ; Include files
  @fundamental_constants
  
  ; Set up error handler
  CATCH, error_status
  IF ( error_status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN, -1L
  ENDIF

  ; Check input
  IF ( N_ELEMENTS( frequency ) EQ 0 ) THEN $
    MESSAGE, 'Input FREQUENCY argument not defined!', $
             /NONAME, /NOPRINT
  ;...Always use double precision
  v = DOUBLE( frequency )
  index = WHERE( v LT ZERO, count )
  IF ( count GT 0 ) THEN $
    MESSAGE, 'Negative frequency input. Units must be cm^-1.', $
             /NONAME, NOPRINT

  ; Convert from cm-1 to GHz
  f = 1.0d-07 * SPEED_OF_LIGHT * v

  ; Done
  CATCH, /CANCEL
  RETURN, f

END
