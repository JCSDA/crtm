;+
;
; NAME:
;       IRwaterCoeff_Info
;
; PURPOSE:
;       Function to return a string containing version and dimension
;       information about a IRwaterCoeff object.
;
; CALLING SEQUENCE:
;       info = Obj->[IRwaterCoeff::]Info( Debug=Debug )
;
; RESULT:
;       info:  String array containing version and dimension information
;              about the IRwaterCoeff object.
;              UNITS:      N/A
;              TYPE:       CHARACTER(*)
;              DIMENSION:  Rank-1
;              ATTRIBUTES: INTENT(OUT)
;
;-
FUNCTION IRwaterCoeff::Info, Debug=debug

  ; Set up
  CRLF = STRING(13B)+STRING(10B)
  @irwatercoeff_parameters
  @irwatercoeff_func_err_handler
  ; ...Check the structure has been allocated
  IF ( NOT self->Associated(Debug=Debug) ) THEN $
    MESSAGE, 'Object has not been allocated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch

  ; Get the information
  self->IRwaterCoeff::Get_Property, $
    Release       = release      , $
    Version       = version      , $
    n_Angles      = n_angles     , $
    n_Frequencies = n_frequencies, $
    n_Wind_Speeds = n_wind_speeds, $
    Debug = debug
  
  ; Write the required data to the string
  info = CRLF + " IRwaterCoeff RELEASE.VERSION: " + $
         STRING(release,version,FORMAT='(i2,".",i2.2)') + $
         STRING(n_angles     ,FORMAT='(", N_ANGLES=",i5)') + $
         STRING(n_frequencies,FORMAT='(", N_FREQUENCIES=",i5)') + $
         STRING(n_wind_speeds,FORMAT='(", N_WIND_SPEEDS=",i5)')
  RETURN, info

END         
