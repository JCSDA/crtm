;+
;
; NAME:
;       Planck_Temperature
;
; PURPOSE:
;       Function to calculate the Planck temperature (brightness temperature)
;       given the wavenumber or wavelength, and radiance.  Calculation is done
;       in double precision.
;
; CALLING SEQUENCE:
;       result = Planck_Temperature( X                    , $  ; Input
;                                    Radiance             , $  ; Input
;                                    Temperature          , $  ; Output
;                                    Wavelength=Wavelength, $  ; Input keyword
;                                    Debug     =Debug       )  ; Input keyword
;
; INPUTS:
;       X:             Spectral ordinate(s) at which the Planck temperature is
;                      to be computed. Can be frequency (wavenumber) or
;                      wavelength.
;                      UNITS:      Inverse centimetres (cm^-1)  [DEFAULT]
;                                  Microns (um)                 [if WAVELENGTH keyword is set]
;                      DIMENSION:  Scalar or rank-1 (Nx1)
;
;       Radiance:      Radiance(s) for which the Planck temperature is
;                      required.
;                      UNITS:      mW/(m2.sr.cm-1)  [DEFAULT]
;                                  W/(m2.sr.um)     [if WAVELENGTH keyword is set]
;                      DIMENSION:  Scalar or rank-1
;                                  This argument is ALWAYS assumed to be a
;                                  spectral quantity. See temperature output
;                                  description for allowed dimensionality.
;
; INPUT KEYWORDS:
;       Wavelength:    Set this keyword to specify wavelength as the spectral
;                      ordinate rather than frequency (the default). See radiance
;                      input description to see how this keyword affects the
;                      radiance units.
;
;       Debug:         Set this keyword for debugging.
;                      If NOT SET => Error handler is enabled. (DEFAULT)
;                         SET     => Error handler is disabled; Routine
;                                    traceback output is enabled.
;
; OUTPUTS:
;       Temperature:   The computed Planck temperature (brightness temperature).
;                      UNITS:      Kelvin
;                      DIMENSION:  Determined by the input data. In the following
;                                  chart N == frequencies
;
;                          Input X      Input Radiance   Output Temperature
;                         dimension       dimension          dimension
;                      ----------------------------------------------------
;                          scalar          scalar              scalar
;                            N             scalar          **NOT ALLOWED**
;                            N               N                   N
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
;       For wavenumber input, the Planck Temperature is calculated using:
;
;                    C2 * wavenumber
;         T = -------------------------------
;                ( C1 * wavenumber^3      )
;              LN( ----------------- +  1 )
;                (          B             )
;
;       For Wavelength input :
;
;                                C2
;          T = -----------------------------------------
;                              (        C1            )
;               Wavelength * LN( ---------------- + 1 )
;                              ( Wavelength^5 * B     )
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
;       K.cm a multiplier of 100 is required. In addition, since 
;       Radiance input is in mW rather than W, an additional scaling
;       of 1/1000 is applied to the input Radiance.
;
;       Wavelength
;       ----------
;       To alter C1 from W.m2 to W/(m2.um-4) a multiplier of 1.0e+24
;       is required. The solid angle is dimensionless and implied, 
;       i.e. W/(m2.um-4) => W/(m2.sr.um-4). Similarly for C2, K.m ->
;       K.um a multiplier of 1.0e+06 is required.
;
;
;       All calculations are done in double precision.
;
;
; EXAMPLE:
;       Wavenumber input with Radiance in mW/(m2.sr.cm-1):
;       --------------------------------------------------
;
;       Calculate a Radiance value for T=300K:
;
;         IDL> Result = Planck_Radiance( 950.0, 300.0, Radiance )
;         IDL> HELP, Radiance
;         Radiance        DOUBLE    =        108.38897
;
;       Calculate the Temperature:
;
;         IDL> Result = Planck_Temperature( 950.0, Radiance, Temperature )
;         IDL> HELP, Temperature
;         Temperature     DOUBLE    =        300.00000
;
;
;       Wavelength input with Radiance in W/(m2.sr.um):
;       -----------------------------------------------
;
;       Calculate a Radiance value for T=300K:
;
;         IDL>  Result = Planck_Radiance( 11.0, 300.0, Radiance, /Wavelength )
;         IDL> HELP, Radiance
;         Radiance        DOUBLE    =        9.5732269
;
;       Calculate the Temperature:
;
;         IDL>  Result = Planck_Temperature( 11.0, Radiance, Temperature, /Wavelength )
;         IDL> HELP, Temperature
;         Temperature     DOUBLE    =        300.00000
;
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, CIMSS/SSEC 16-Jul-1996
;                       paul.vandelst@noaa.gov
;
;-

FUNCTION Planck_Temperature, Input_X              , $  ; Input
                             Input_Radiance       , $  ; Input
                             Temperature          , $  ; Output
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
  r = DOUBLE(Input_Radiance)

  ; Check sizes X and Radiance input
  IF ( N_ELEMENTS(x) NE N_ELEMENTS(r) ) THEN $
    MESSAGE, 'Inconsistent X and RADIANCE sizes', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch

  ; Check values
  loc = WHERE(x LT Tolerance OR FINITE(x) EQ 0, count)
  IF ( count GT 0 ) THEN $
    MESSAGE, 'Input X-values < or = 0.0, NaN, or Inf found.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch

  loc = WHERE(r LT Tolerance OR FINITE(r) EQ 0, count)
  IF ( count GT 0 ) THEN $
    MESSAGE, 'Input Radiances < or = 0.0, NaN, or Inf found.', $
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


  ; Calculate Planck temperature
  ; ----------------------------
  Logarithm   = ALOG((Fk1 / (r / R_SCALE[Unit_Index])) + ONE)
  Temperature = Fk2 / Logarithm


  ; Done
  ; ----
  CATCH, /CANCEL
  RETURN, SUCCESS

END
