;+
; NAME:
;       OSRF::Set_Property
;
; PURPOSE:
;       The OSRF::Set_Property procedure method sets the value of a property
;       or group of properties for this object.
;
; CALLING SEQUENCE:
;       Obj->[OSRF::]Set_Property, $
;         [Band]                                     , $  ; Optional input
;         Debug                = Debug               , $  ; Input keyword
;         Version              = Version             , $  ; Input keyword
;         Sensor_Id            = Sensor_Id           , $  ; Input keyword
;         WMO_Satellite_ID     = WMO_Satellite_ID    , $  ; Input keyword
;         WMO_Sensor_ID        = WMO_Sensor_ID       , $  ; Input keyword
;         Sensor_Type          = Sensor_Type         , $  ; Input keyword
;         Channel              = Channel             , $  ; Input keyword
;         Convolved_R          = Convolved_R         , $  ; Input keyword
;         Convolved_T          = Convolved_T         , $  ; Input keyword
;         Frequency            = Frequency           , $  ; Input keyword
;         Response             = Response            , $  ; Input keyword
;         Radiance             = Radiance                 ; Input keyword
;
; OPTIONAL INPUT ARGUMENTS:
;       Band:                  The 1-based band number for which the frequency and
;                              response refer to. If not specified, default value is 1.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  SCALAR
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
; INPUT KEYWORDS:
;       Debug:                 Set this keyword for debugging.
;                              If NOT SET => Error handler is enabled. (DEFAULT)
;                                 SET     => Error handler is disabled; Routine
;                                            traceback output is enabled.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Version:               The version number of the SRF data.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Sensor_ID:             A character string identifying the sensor and
;                              satellite platform used to contruct filenames.
;                              UNITS:      N/A
;                              TYPE:       CHARACTER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       WMO_Satellite_ID:      The WMO code used to identify satellite platforms.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       WMO_Sensor_ID:         The WMO code used to identify sensors.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Sensor_Type:           The flag indicating the type of sensor (IR, MW, etc)
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Channel:               The sensor channel for the currenobject.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Convolved_R:           Convolved radiance from LBL or Planck radiance method (if called)
;                              UNITS:      mW/(m^2.sr.cm^-1)
;                              TYPE:       REAL
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Convolved_T:           Brightness temperature corresponding to Convolved_R.
;                              UNITS:      Kelvin
;                              TYPE:       REAL
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Frequency:             The frequency grid for an SRF band.
;                              Used in conjunction with the Band keyword argument.
;                              UNITS:      Inverse centimetres (cm^-1)
;                              TYPE:       REAL
;                              DIMENSION:  n_Points
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Response:              The response data for an SRF band.
;                              Used in conjunction with the Band keyword argument.
;                              UNITS:      N/A
;                              TYPE:       REAL
;                              DIMENSION:  n_Points
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Radiance:              Radiance data for an SRF band.
;                              NOTE: This component is automatically filled in the
;                                oSRF::Compute_Planck_Radiance
;                              and
;                                oSRF::Compute_Polychromatic_Coefficients
;                              methods.
;                              Used in conjunction with the Band keyword argument.
;                              UNITS:      N/A
;                              TYPE:       REAL
;                              DIMENSION:  n_Points
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
; INCLUDE FILES:
;       osrf_parameters: Include file containing OSRF specific
;                        parameter value definitions.
;
;       osrf_pro_err_handler: Error handler code for OSRF procedures.
;
; EXAMPLE:
;       Given a valid, allocated, OSRF object, x, assign various OSRF
;       components values like so,
;
;         IDL> x.Set_Property, Sensor_Id=sid, Channel=ch
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 20-Apr-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO OSRF::Set_Property, $
  Band                               , $  ; Optional input
  Debug            = Debug           , $  ; Input keyword
  Version          = Version         , $  ; Input keyword
  Sensor_Id        = Sensor_Id       , $  ; Input keyword
  WMO_Satellite_Id = WMO_Satellite_Id, $  ; Input keyword
  WMO_Sensor_Id    = WMO_Sensor_Id   , $  ; Input keyword
  Sensor_Type      = Sensor_Type     , $  ; Input keyword
  Channel          = Channel         , $  ; Input keyword
  Convolved_R      = Convolved_R     , $  ; Input keyword
  Convolved_T      = Convolved_T     , $  ; Input keyword
  Frequency        = Frequency       , $  ; Input keyword
  Response         = Response        , $  ; Input keyword
  ; The following keywords are undocumented
  Special    = special   , $ ; Must be set for the following
  Integral   = integral  , $
  Flags      = flags     , $
  f0         = f0        , $
  poly_Tdata = poly_tdata
  
  
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
  
  
  ; Set scalar/array data
  IF ( N_ELEMENTS(Version         ) GT 0 ) THEN self.Version          = Version         
  IF ( N_ELEMENTS(Sensor_Id       ) GT 0 ) THEN self.Sensor_Id        = Sensor_Id       
  IF ( N_ELEMENTS(Sensor_Type     ) GT 0 ) THEN self.Sensor_Type      = Sensor_Type
  IF ( N_ELEMENTS(WMO_Satellite_ID) GT 0 ) THEN self.WMO_Satellite_ID = WMO_Satellite_ID
  IF ( N_ELEMENTS(WMO_Sensor_ID   ) GT 0 ) THEN self.WMO_Sensor_ID    = WMO_Sensor_ID   
  IF ( N_ELEMENTS(Channel         ) GT 0 ) THEN self.Channel          = Channel         
  IF ( N_ELEMENTS(Convolved_R     ) GT 0 ) THEN self.Convolved_R      = Convolved_R
  IF ( N_ELEMENTS(Convolved_T     ) GT 0 ) THEN self.Convolved_T      = Convolved_T


  ; Set frequency data
  n_points = N_ELEMENTS(Frequency)
  IF ( n_points GT 0 ) THEN BEGIN
    ; ...Check point numbering is consistent
    IF ( n_points NE self.n_Points[_band] ) THEN $
      MESSAGE, 'Input frequency array different size from that expected for band ' + $
               STRTRIM(_band,2), $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    ; ...Assign frequency data
    self.f1[_band]        = Frequency[0]
    self.f2[_band]        = Frequency[n_points-1]
    self.Frequency[_band] = Frequency
    IF ( KEYWORD_SET(debug) ) THEN $
      MESSAGE, 'Frequency properties for band ' + STRTRIM(_band,2) + ' have been set', $
               /INFORMATIONAL
  ENDIF


  ; Set response data
  n_points = N_ELEMENTS(Response)
  IF ( n_points GT 0 ) THEN BEGIN
    ; ...Check point numbering is consistent
    IF ( n_points NE self.n_Points[_band] ) THEN $
      MESSAGE, 'Input response array different size from that allocated for band ' + $
               STRTRIM(_band,2), $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    ; ...Assign response data
    self.Response[_band] = Response
    IF ( KEYWORD_SET(debug) ) THEN $
      MESSAGE, 'Response property for band ' + STRTRIM(_band,2) + ' have been set', $
               /INFORMATIONAL
  ENDIF


  ; The special cases
  IF ( KEYWORD_SET(special) ) THEN BEGIN
    IF ( N_ELEMENTS(integral  ) GT 0 ) THEN self.Integral = integral     
    IF ( N_ELEMENTS(flags     ) GT 0 ) THEN self.Flags    = flags        
    IF ( N_ELEMENTS(f0        ) GT 0 ) THEN self.f0       = f0
    IF ( N_ELEMENTS(poly_tdata) GT 0 ) THEN BEGIN
      *self.T    = poly_tdata["T"]
      *self.Teff = poly_tdata["Teff"]
      *self.Tfit = poly_tdata["Tfit"]
    ENDIF
  ENDIF
  
END
