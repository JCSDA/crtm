;+
; NAME:
;       OSRF::Inspect
;
; PURPOSE:
;       The OSRF::Inspect procedure method outputs information
;       about the current OSRF object.
;
; CALLING SEQUENCE:
;       Obj->[OSRF::]Inspect, $
;         Verbose=Verbose, &  ; Input keyword
;         Debug  =Debug       ; Input keyword
;
; INPUT KEYWORD PARAMETERS:
;       Verbose:     Set this keyword for more verbose output.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Debug:       Set this keyword for debugging.
;                    If NOT SET => Regular output. (DEFAULT)
;                       SET     => Information about all currently compiled
;                                  routines and their arguments are output.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
;
; EXAMPLE:
;       Inspect the contents of a OSRF object, x:
;
;         IDL> x->Inspect, /Verbose
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 20-Apr-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO OSRF::Inspect, $
  Verbose=Verbose, $  ; Input keyword
  Debug=Debug         ; Input keyword

  COMPILE_OPT HIDDEN

  IF ( KEYWORD_SET(Debug) ) THEN HELP, /ROUTINES
  HELP, self, /OBJECTS

  IF ( KEYWORD_SET(Verbose) ) THEN BEGIN

    ; Get the number of bands
    self.Get_Property, n_Bands=n_bands, Debug=Debug
    PRINT, FORMAT='(/,"Number of passbands: ",i1)', n_bands

    
    ; Channel coefficients
    self.Get_Property, Planck_Coeffs=planck_coeffs, $
                       Polychromatic_Coeffs=polychromatic_coeffs, $
                       Debug=Debug
    PRINT, FORMAT='(/,"Coefficient data:")'
    PRINT, FORMAT='("PLANCK_COEFFS:        ",5(1x,e13.6))', planck_coeffs
    PRINT, FORMAT='("POLYCHROMATIC_COEFFS: ",5(1x,e13.6))', polychromatic_coeffs

    
    ; The band definition data
    PRINT, FORMAT='(/,"Band definition data:")'
    FOR n = 0, n_bands-1 DO BEGIN
      band = n+1
      self.Get_Property, $
        band, $
        f1=f1, $
        f2=f2, $
        n_Points=n_Points, $
        Bandwidth=bandwidth, $
        Debug=Debug
      PRINT, band, FORMAT='(2x,"Band ",i1," definition data:")'
      PRINT, FORMAT='("F1:       ",e13.6)', f1
      PRINT, FORMAT='("F2:       ",e13.6)', f2
      PRINT, FORMAT='("N_POINTS: ",i13)', n_Points
      PRINT, FORMAT='("WIDTH   : ",e13.6)', bandwidth
    ENDFOR


    ; The band response data
    PRINT, FORMAT='(/,"Band response data:")'
    FOR n = 0, n_bands-1 DO BEGIN
      band = n+1
      self.Get_Property, band, Frequency=frequency, Response=response, Debug=Debug
      PRINT, band, FORMAT='(2x,"Band ",i1," frequency and response:")'
      HELP, band, frequency, response
    ENDFOR


    ; The band radiance data
    PRINT, FORMAT='(/,"Band radiance data:")'
    FOR n = 0, n_bands-1 DO BEGIN
      band = n+1
      self.Get_Property, band, Frequency=frequency, Radiance=radiance, Debug=Debug
      PRINT, band, FORMAT='(2x,"Band ",i1," frequency and radiance:")'
      HELP, band, frequency, radiance
    ENDFOR


    ; The flag status
    PRINT, FORMAT='(/,"Flag settings:")'
    self.Display_Flags, Debug=Debug
    
  ENDIF 
  
END
