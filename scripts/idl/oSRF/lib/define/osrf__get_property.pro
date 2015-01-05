;+
; NAME:
;       OSRF::Get_Property
;
; PURPOSE:
;       The OSRF::Get_Property procedure method retrieves the value of a property
;       or group of properties for this object.
;
; CALLING SEQUENCE:
;       Obj->[OSRF::]Get_Property, $
;         [Band]                                     , $  ; Optional input 
;         Debug                = Debug               , $  ; Input keyword
;         n_Bands              = n_Bands             , $  ; Output keyword
;         Version              = Version             , $  ; Output keyword
;         Sensor_Id            = Sensor_Id           , $  ; Output keyword
;         WMO_Satellite_ID     = WMO_Satellite_ID    , $  ; Output keyword
;         WMO_Sensor_ID        = WMO_Sensor_ID       , $  ; Output keyword
;         Sensor_Type          = Sensor_Type         , $  ; Output keyword
;         Channel              = Channel             , $  ; Output keyword
;         Integral             = Integral            , $  ; Output keyword
;         Flags                = Flags               , $  ; Output keyword
;         f0                   = f0                  , $  ; Output keyword
;         Planck_Coeffs        = Planck_Coeffs       , $  ; Output keyword
;         Polychromatic_Coeffs = Polychromatic_Coeffs, $  ; Output keyword
;         Convolved_R          = Convolved_R         , $  ; Output keyword
;         Convolved_T          = Convolved_T         , $  ; Output keyword
;         n_Points             = n_Points            , $  ; Output keyword
;         f1                   = f1                  , $  ; Output keyword
;         f2                   = f2                  , $  ; Output keyword
;         Frequency            = Frequency           , $  ; Output keyword
;         Response             = Response            , $  ; Output keyword
;         Bandwidth            = Bandwidth           , $  ; Output keyword
;         Radiance             = Radiance            , $  ; Output keyword
;         poly_Tdata           = poly_Tdata          , $  ; Output keyword
;         n_Temperatures       = n_Temperatures      , $  ; Output keyword
;         pRef                 = pRef                     ; Output keyword
;
; OPTIONAL INPUT ARGUMENTS:
;       Band:                  The 1-based band number for which the frequency and
;                              response refer to. If not specified, default value is 1.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  SCALAR
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
; INPUT KEYWORD PARAMETERS:
;       Debug:                 Set this keyword for debugging.
;                              If NOT SET => Error handler is enabled. (DEFAULT)
;                                 SET     => Error handler is disabled; Routine
;                                            traceback output is enabled.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
; OUTPUT KEYWORD PARAMETERS:
;       n_Bands:               The number of bands dimension of the SRF.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  SCALAR
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Version:               The version number of the SRF data.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Sensor_ID:             A character string identifying the sensor and
;                              satellite platform used to contruct filenames.
;                              UNITS:      N/A
;                              TYPE:       CHARACTER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       WMO_Satellite_ID:      The WMO code used to identify satellite platforms.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       WMO_Sensor_ID:         The WMO code used to identify sensors.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Sensor_Type:           The flag indicating the type of sensor (IR, MW, etc)
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Channel:               The sensor channel for the current object.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Integral:              The integrated SRF value.
;                              UNITS:      N/A
;                              TYPE:       REAL
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Flags:                 Bit flags set/cleared during SRF processing.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       f0:                    The central frequency of the SRF.
;                              UNITS:      Inverse centimetres (cm^-1) or gigahertz (GHz)
;                              TYPE:       REAL
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Planck_Coeffs:         Vector of Planck function coefficients for the SRF.
;                              UNITS:      Variable
;                              TYPE:       REAL
;                              DIMENSION:  Rank-1
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Polychromatic_Coeffs:  Vector of polychromatic correction coefficient for the SRF.
;                              UNITS:      Variable
;                              TYPE:       REAL
;                              DIMENSION:  Rank-1
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Convolved_R:           Convolved radiance from LBL or Planck radiance method (if called)
;                              UNITS:      mW/(m^2.sr.cm^-1)
;                              TYPE:       REAL
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Convolved_T:           Brightness temperature corresponding to Convolved_R.
;                              UNITS:      Kelvin
;                              TYPE:       REAL
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       n_Points:              The number of points used to represent a band of
;                              the SRF.
;                              If the Band argument:
;                                - is specified:     The value for that band is returned.
;                                - is NOT specified: An array of values for all bands is returned.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       f1:                    The begin frequency of the SRF band.
;                              Used in conjunction with the Band argument.
;                              UNITS:      Inverse centimetres (cm^-1)
;                              TYPE:       REAL
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       f2:                    The vector of SRF band end frequencies.
;                              Used in conjunction with the Band argument.
;                              UNITS:      Inverse centimetres (cm^-1)
;                              TYPE:       REAL
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Bandwidth:             The bandwidth for an SRF passband.
;                              Used in conjunction with the Band argument.
;                              UNITS:      N/A
;                              TYPE:       REAL
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Frequency:             The frequency grid for an SRF band.
;                              Used in conjunction with the Band argument.
;                              UNITS:      Inverse centimetres (cm^-1)
;                              TYPE:       REAL
;                              DIMENSION:  n_Points
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Response:              The response data for an SRF band.
;                              Used in conjunction with the Band argument.
;                              UNITS:      N/A
;                              TYPE:       REAL
;                              DIMENSION:  n_Points
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Radiance:              Radiance data for an SRF band.
;                              Used in conjunction with the Band argument.
;                              UNITS:      N/A
;                              TYPE:       REAL
;                              DIMENSION:  n_Points
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       poly_Tdata:            Hash of temperature data used to generate the
;                              polychromatic coefficients. The hash keys are:
;                                "T"    - the true temperature (X-data).
;                                "Teff" - the effective temperature (Y-data).
;                                "Tfit" - the polynomial fit to the Teff=f(T) data. 
;                              UNITS:      Kelvin (hash values)
;                              TYPE:       HASH
;                              DIMENSION:  N/A
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       n_Temperatures:        The number of temperature points used in the
;                              polychromatic coefficient fits.
;                              This is syntactic sugar to avoid having to 
;                              retrieve the poly_Tdata itself and then figure
;                              out how many data points there are.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  SCALAR
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       pRef:                  Graphics plot reference for an SRF band.
;                              Used in conjunction with the Band argument.
;                              UNITS:      N/A
;                              TYPE:       Graphics PLOT object
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
; INCLUDE FILES:
;       osrf_parameters: Include file containing OSRF specific
;                        parameter value definitions.
;
;       osrf_pro_err_handler: Error handler code for OSRF procedures.
;
; EXAMPLE:
;       Given a valid oSRF object, x, obtain various oSRF
;       components values like so,
;
;         IDL> x.Get_Property, Sensor_Id=sid, Channel=ch
;         IDL> HELP, sid, ch
;         SID             STRING    = 'hirs4_n18           '
;         CH              LONG      =            7
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 22-May-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO OSRF::Get_Property, $
  Band                                       , $  ; Optional input
  Debug                = Debug               , $  ; Input keyword
  n_Bands              = n_Bands             , $  ; Output keyword
  Version              = Version             , $  ; Output keyword
  Sensor_Id            = Sensor_Id           , $  ; Output keyword
  WMO_Satellite_ID     = WMO_Satellite_ID    , $  ; Output keyword
  WMO_Sensor_ID        = WMO_Sensor_ID       , $  ; Output keyword
  Sensor_Type          = Sensor_Type         , $  ; Output keyword
  Channel              = Channel             , $  ; Output keyword
  Integral             = Integral            , $  ; Output keyword
  Flags                = Flags               , $  ; Output keyword
  f0                   = f0                  , $  ; Output keyword
  Planck_Coeffs        = Planck_Coeffs       , $  ; Output keyword
  Polychromatic_Coeffs = Polychromatic_Coeffs, $  ; Output keyword
  Convolved_R          = Convolved_R         , $  ; Output keyword
  Convolved_T          = Convolved_T         , $  ; Output keyword
  n_Points             = n_Points            , $  ; Output keyword
  f1                   = f1                  , $  ; Output keyword
  f2                   = f2                  , $  ; Output keyword
  Bandwidth            = Bandwidth           , $  ; Output keyword
  Frequency            = Frequency           , $  ; Output keyword
  Response             = Response            , $  ; Output keyword
  Radiance             = Radiance            , $  ; Output keyword
  poly_Tdata           = poly_Tdata          , $  ; Output keyword
  n_Temperatures       = n_Temperatures      , $  ; Output keyword
  pRef                 = pRef                , $  ; Output keyword
  wRef                 = wRef                , $  ; Output keyword
  tpRef                = tpRef                , $  ; Output keyword
  twRef                = twRef                     ; Output keyword

  ; Set up
  COMPILE_OPT HIDDEN
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler
 
  
  ; Check if object has been allocated
  IF ( ~ self.Associated(Debug=Debug) ) THEN $
    MESSAGE, "OSRF object has not been allocated.", $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Check band argument
  _band = 1L  ; Default
  IF ( N_ELEMENTS(Band) GT 0 ) THEN BEGIN
    _band = LONG(Band[0])
    IF ( _band LT 1 OR _band GT self.n_Bands ) THEN $
      MESSAGE, "Invalid band specified.", $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ENDIF


  ; Get Scalar/Array data
  IF ( ARG_PRESENT(n_Bands             ) ) THEN n_Bands              = self.n_Bands
  IF ( ARG_PRESENT(Version             ) ) THEN Version              = self.Version         
  IF ( ARG_PRESENT(Sensor_Id           ) ) THEN Sensor_Id            = self.Sensor_Id       
  IF ( ARG_PRESENT(WMO_Satellite_ID    ) ) THEN WMO_Satellite_ID     = self.WMO_Satellite_ID
  IF ( ARG_PRESENT(WMO_Sensor_ID       ) ) THEN WMO_Sensor_ID        = self.WMO_Sensor_ID   
  IF ( ARG_PRESENT(Sensor_Type         ) ) THEN Sensor_Type          = self.Sensor_Type     
  IF ( ARG_PRESENT(Channel             ) ) THEN Channel              = self.Channel         
  IF ( ARG_PRESENT(Integral            ) ) THEN Integral             = self.Integral
  IF ( ARG_PRESENT(Flags               ) ) THEN Flags                = self.Flags
  IF ( ARG_PRESENT(f0                  ) ) THEN f0                   = self.f0              
  IF ( ARG_PRESENT(Planck_Coeffs       ) ) THEN Planck_Coeffs        = self.Planck_Coeffs
  IF ( ARG_PRESENT(Polychromatic_Coeffs) ) THEN Polychromatic_Coeffs = self.Polychromatic_Coeffs
  IF ( ARG_PRESENT(Convolved_R         ) ) THEN Convolved_R          = self.Convolved_R
  IF ( ARG_PRESENT(Convolved_T         ) ) THEN Convolved_T          = self.Convolved_T
  ; ...Assemble polychromatic temperature data 
  IF ( ARG_PRESENT(poly_Tdata) ) THEN $
    poly_Tdata = HASH("T"   ,*self.T   , $
                      "Teff",*self.Teff, $
                      "Tfit",*self.Tfit  )
  IF ( ARG_PRESENT(n_Temperatures) ) THEN n_Temperatures = N_ELEMENTS(*self.T)

  
  ; Get band specific data  
  IF ( ARG_PRESENT(f1       ) ) THEN f1        = self.f1[_band]       
  IF ( ARG_PRESENT(f2       ) ) THEN f2        = self.f2[_band]       
  IF ( ARG_PRESENT(Bandwidth) ) THEN Bandwidth = self.Bandwidth[_band]
  IF ( ARG_PRESENT(Frequency) ) THEN Frequency = self.Frequency[_band]
  IF ( ARG_PRESENT(Response ) ) THEN Response  = self.Response[_band]
  IF ( ARG_PRESENT(Radiance ) ) THEN Radiance  = self.Radiance[_band]


  ; Plot variables
  IF ( ARG_PRESENT(pRef ) ) THEN pRef  = self.pRef[_band]
  IF ( ARG_PRESENT(wRef ) ) THEN wRef  = self.wRef
  IF ( ARG_PRESENT(tpRef) ) THEN tpRef = self.tpRef
  IF ( ARG_PRESENT(twRef) ) THEN twRef = self.twRef


  ; Band number of points is special case
  IF ( ARG_PRESENT(n_Points) ) THEN BEGIN
    IF ( N_ELEMENTS(Band) EQ 0 ) THEN BEGIN
      n_Points = []
      FOR i = 1, self.n_Bands DO n_Points = [n_Points,LONG(self.n_Points[i])]
    ENDIF ELSE BEGIN
      n_Points = LONG(self.n_Points[_band])
    ENDELSE  
  ENDIF
  
END
