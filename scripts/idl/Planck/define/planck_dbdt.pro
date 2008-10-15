;+
;
; NAME:
;       Planck_dBdT
;
; PURPOSE:
;       Function to calculate the derivative of the Planck radiance equation
;       with respect to temperature given the wavenumber or wavelength, and
;       temperature.  Calculation is done in double precision.
;
; CALLING SEQUENCE:
;       result = Planck_dBdT( X                    , $  ; Input
;                             Temperature          , $  ; Input
;                             dBdT                 , $  ; Output
;                             Wavelength=Wavelength, $  ; Input keyword
;                             Debug     =Debug       )  ; Input keyword
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
;                      ordinate rather than frequency (the default). See dBdT
;                      output description to see how this keyword affects the
;                      dBdT units.
;
;       Debug:         Set this keyword for debugging.
;                      If NOT SET => Error handler is enabled. (DEFAULT)
;                         SET     => Error handler is disabled; Routine
;                                    traceback output is enabled.
;
; OUTPUTS:
;       dBdT:          The computed derivative of the Planck radiance with
;                      respect to temperature.
;                      UNITS:      mW/(m2.sr.cm-1.K)  [DEFAULT]
;                                  W/(m2.sr.um.K)     [if WAVELENGTH keyword is set]
;                      DIMENSION:  Determined by the input data. In the following
;                                  chart N == frequencies
;
;                          Input X     Input TEMPERATURE    Output dBdT
;                         dimension       dimension          dimension
;                      ----------------------------------------------------
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
;       For wavenumber input, the Planck Radiance differential with respect
;       to Temperature is calculated using :
;
;                                               ( C2 * wavenumber )
;                   C1 * C2 * wavenumber^4 * EXP( --------------- )
;                                               (        T        )
;          dB/dT = -------------------------------------------------
;                       {     [    ( C2 * wavenumber )     ] }^2
;                       { T * [ EXP( --------------- ) - 1 ] }
;                       {     [    (        T        )     ] }
;
;       For Wavelength input :
;                                            (       C2       )
;                               C1 * C2 * EXP( -------------- )
;                                            ( Wavelength * T )
;          dB/dT = --------------------------------------------------------
;                                  {     [    (       C2       )     ] }^2
;                   Wavelength^6 * { T * [ EXP( -------------- ) - 1 ] }
;                                  {     [    ( Wavelength * T )     ] }
;
;       C1 and C2 are determined using:
;          C1 = 2*h*c*c  [W.m2]
;          C2 = h*c/k    [K.m]
;
;       The fundamental and derived constants used are defined in the
;       include file fundamental_constants.pro
;
;       A scaling is applied to the Planck Radiance results to return
;       Radiances in the units of mW/(m2.sr.cm-1) for wavenumber input
;       or W/(m2.sr.micron) for wavelength input.
;
;       WAVENUMBER
;       ----------
;       To alter C1 from W.m2 to W/(m2.cm-4) a multiplier of 1.0e+08
;       is required. The solid angle is dimensionless and implied,
;       i.e. W/(m2.cm-4) => W/(m2.st.cm-4). Similarly for C2, K.m -> 
;       K.cm a multiplier of 100 is required. In addition, to return
;       mW rather than W, an additional scaling of 1000 is applied.
;
;       Wavelength
;       ----------
;       To alter C1 from W.m2 to W/(m2.um-4) a multiplier of 1.0e+24
;       is required. The solid angle is dimensionless and implied, 
;       i.e. W/(m2.um-4) => W/(m2.sr.um-4). Similarly for C2, K.m ->
;       K.um a multiplier of 1.0e+06 is required.
;
;       All calculations are done in double precision.
;
; EXAMPLE:
;       Wavenumber input providing dB/dT in mW/(m2.sr.cm-1.K):
;       ------------------------------------------------------
;
;         IDL> Result = Planck_dBdT( 950.0, 300.0, dbdt )
;         IDL> HELP, dbdt
;         DBDT            DOUBLE    =        1.6636127
;
;
;       Wavelength input providing dB/dT in W/(m2.sr.um.K):
;       ---------------------------------------------------
;
;         IDL> Result = Planck_dBdT( 11.0, 300.0, dbdt, /Wavelength )
;         IDL> HELP, dbdt
;         DBDT            DOUBLE    =        0.14093186
;
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, CIMSS/SSEC 04-Nov-1996
;                       paul.vandelst@noaa.gov
;
;-

FUNCTION Planck_dBdT, Input_X              , $  ; Input
                      Input_Temperature    , $  ; Input
                      dBdT                 , $  ; Output
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
    Fk1 = C1_SCALE[Unit_Index] * C1 * $
          C2_SCALE[Unit_Index] * C2 * (x^4)
    Fk2 = C2_SCALE[Unit_Index] * C2 * x
  ENDIF ELSE BEGIN
    Fk1 = C1_SCALE[Unit_Index] * C1 * $
          C2_SCALE[Unit_Index] * C2 / (x^6)
    Fk2 = C2_SCALE[Unit_Index] * C2 / x
  ENDELSE 


  ; Calculate the Planck radiance derivative wrt temperature
  ; --------------------------------------------------------
  Exponential = EXP(Fk2 / t)
  dBdT = R_SCALE[Unit_Index] * Fk1 * Exponential / ( t * ( Exponential - ONE ) )^2


  ; Done
  ; ----
  CATCH, /CANCEL
  RETURN, SUCCESS


END
