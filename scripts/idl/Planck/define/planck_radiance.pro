;+
;
; NAME:
;       Planck_Radiance
;
; PURPOSE:
;       Function to calculate the Planck radiance given the wavenumber or
;       wavelength, and temperature.  Calculation is done in double precision.
;
; CALLING SEQUENCE:
;       result = Planck_Radiance( X                    , $  ; Input
;                                 Temperature          , $  ; Input
;                                 Radiance             , $  ; Output
;                                 Wavelength=Wavelength, $  ; Input keyword
;                                 Debug     =Debug       )  ; Input keyword
;
; INPUTS:
;       X:             Spectral ordinate(s) at which the Planck radiance is
;                      to be computed. Can be frequency (wavenumber) or
;                      wavelength.
;                      UNITS:      Inverse centimetres (cm^-1)  [DEFAULT]
;                                  Microns (um)                 [if WAVELENGTH keyword is set]
;                      DIMENSION:  Scalar or rank-1
;
;       Temperature:   Temperature(s) at which the Planck radiance is to be
;                      computed.
;                      UNITS:      Kelvin
;                      DIMENSION:  Scalar or rank-1
;                                  See radiance output description for allowed
;                                  dimensionality.
;
; INPUT KEYWORDS:
;       Wavelength:    Set this keyword to specify wavelength as the spectral
;                      ordinate rather than frequency (the default). See radiance
;                      output description to see how this keyword affects the
;                      radiance units.
;
;       Debug:         Set this keyword for debugging.
;                      If NOT SET => Error handler is enabled. (DEFAULT)
;                         SET     => Error handler is disabled; Routine
;                                    traceback output is enabled.
;
; OUTPUTS:
;       Radiance:      The computed Planck radiance.
;                      UNITS:      mW/(m2.sr.cm-1)  [DEFAULT]
;                                  W/(m2.sr.um)     [if WAVELENGTH keyword is set]
;                      DIMENSION:  Determined by the input data. In the following
;                                  chart N == frequencies
;
;                          Input X     Input TEMPERATURE    Output RADIANCE
;                         dimension       dimension            dimension
;                      ------------------------------------------------------
;                          scalar          scalar                scalar
;                            N             scalar                  N
;                            N               N                     N
;
; FUNCTION RESULT:
;       Result:        The return value is an integer defining the error
;                      status. The error codes are defined in the error_codes
;                      include file.
;                      If == SUCCESS the Planck calculation was successful
;                         == FAILURE an unrecoverable error occurred
;                      UNITS:      N/A
;                      TYPE:       INTEGER
;                      DIMENSION:  Scalar
;
; INCLUDE FILES:
;       error_codes:            Include file containing error code definitions.
;
;       fundamental_constants:  Include file containing various fundamental
;                               physical constants.
;
; PROCEDURE:
;       For wavenumber input, the Planck radiance is calculated using:
;
;                   c1 * wavenumber^3
;         B =  ----------------------------
;                  ( c2 * wavenumber )
;               EXP( --------------- ) - 1
;                  (        T        )
;
;       For wavelength input:
;
;                                    c1
;         B = --------------------------------------------------
;                             [    (        c2      )     ]
;              wavelength^5 * [ EXP( -------------- ) - 1 ]
;                             [    ( wavelength * T )     ]

;       c1 and c2 are determined using:
;          c1 = 2*h*c*c  [W.m2]
;          c2 = h*c/k    [K.m]
;
;       The fundamental and derived constants used are defined in the
;       include file fundamental_constants.pro
;
;       A scaling is applied to the Planck radiance results to return
;       radiances in the units of mW/(m2.sr.cm-1) for wavenumber input
;       or W/(m2.sr.micron) for wavelength input.
;
;       WAVENUMBER
;       ----------
;       To alter c1 from W.m2 to W/(m2.cm-4) a multiplier of 1.0e+08
;       is required. The solid angle is dimensionless and implied,
;       i.e. W/(m2.cm-4) => W/(m2.st.cm-4). Similarly for c2, K.m -> 
;       K.cm a multiplier of 100 is required. In addition, to return
;       mW rather than W, an additional scaling of 1000 is applied.
;
;       WAVELENGTH
;       ----------
;       To alter c1 from W.m2 to W/(m2.um-4) a multiplier of 1.0e+24
;       is required. The solid angle is dimensionless and implied, 
;       i.e. W/(m2.um-4) => W/(m2.sr.um-4). Similarly for c2, K.m ->
;       K.um a multiplier of 1.0e+06 is required.
;
;       All calculations are done in double precision.
;
; EXAMPLE:
;       Wavenumber input providing radiance in mW/(m2.sr.cm-1):
;       -------------------------------------------------------
;
;         IDL> Result=Planck_Radiance( 950.0, 300.0, radiance )
;         IDL> HELP, radiance
;         RADIANCE        DOUBLE    =        108.38897
;
;
;       Wavelength input providing radiance in W/(m2.sr.um):
;       ----------------------------------------------------
;
;         IDL> Result=Planck_Radiance( 11.0, 300.0, radiance, /wavelength )
;         IDL> HELP, radiance
;         RADIANCE        DOUBLE    =        9.5732269
;
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, CIMSS/SSEC 16-Jul-1996
;                       paul.vandelst@noaa.gov
;
;-

FUNCTION Planck_Radiance, Input_X              , $  ; Input
                          Input_Temperature    , $  ; Input
                          Radiance             , $  ; Output
                          Wavelength=Wavelength, $  ; Input keyword
                          Debug     =Debug          ; Input keyword

  ; Set up
  ; ------
  ; Include constants
  @fundamental_constants
  @planck_constants

  ; Define tolerance value
  Tolerance = (MACHAR(/DOUBLE)).EPS

  ; Error handler
  @error_codes
  IF ( KEYWORD_SET(Debug) ) THEN BEGIN
    MESSAGE, '--> Entered.', /INFORMATIONAL
    MsgSwitch = 0
  ENDIF ELSE BEGIN
    CATCH, Error_Status
    IF ( Error_Status NE 0 ) THEN BEGIN
      CATCH, /CANCEL
      MESSAGE, !ERROR_STATE.MSG, /CONTINUE
      RETURN, FAILURE
    ENDIF
    MsgSwitch = 1
  ENDELSE

  ; All calcs are done in double precision
  x = DOUBLE(Input_X)
  t = DOUBLE(Input_Temperature)

  ; Check sizes X and Temperature input
  IF ( N_ELEMENTS(x) NE N_ELEMENTS(t) ) THEN $
    t = REPLICATE(t[0],N_ELEMENTS(x))

  ; Check values
  loc = WHERE(x LT Tolerance OR FINITE(x) EQ 0, count)
  IF ( count GT 0 ) THEN $
    MESSAGE, 'Input X-values < or = 0.0, NaN, or Inf found.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch

  loc = WHERE(t LT Tolerance OR FINITE(t) EQ 0, count)
  IF ( count GT 0 ) THEN $
    MESSAGE, 'Input Temperatures < or = 0.0, NaN, or Inf found.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch

  ; Default units are in terms of frequency...
  Frequency_Units = TRUE
  Unit_Index      = FREQUENCY_INDEX
  ; ...unless the Wavelength keyword is set
  IF ( KEYWORD_SET(Wavelength) ) THEN BEGIN
    Frequency_Units = FALSE
    Unit_Index      = WAVELENGTH_INDEX
  ENDIF
    

  ; Compute FK1 and FK2 quantities
  ; ------------------------------
  IF ( Frequency_Units ) THEN BEGIN
    Fk1 = C1_SCALE[Unit_Index] * C1 * (x^3)
    Fk2 = C2_SCALE[Unit_Index] * C2 * x
  ENDIF ELSE BEGIN
    Fk1 = C1_SCALE[Unit_Index] * C1 / (x^5)
    Fk2 = C2_SCALE[Unit_Index] * C2 / x
  ENDELSE 


  ; Calculate Planck radiance
  ; -------------------------
  Exponential = EXP(Fk2 / t)
  Radiance    = R_SCALE[Unit_Index] * Fk1 / ( Exponential - ONE )


  ; Done
  ; ----
  CATCH, /CANCEL
  RETURN, SUCCESS


END
