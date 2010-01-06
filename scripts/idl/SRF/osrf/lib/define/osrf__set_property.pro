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
;         Integral             = Integral            , $  ; Input keyword
;         Flags                = Flags               , $  ; Input keyword
;         f0                   = f0                  , $  ; Input keyword
;         Planck_Coeffs        = Planck_Coeffs       , $  ; Input keyword
;         Polychromatic_Coeffs = Polychromatic_Coeffs, $  ; Input keyword
;         Convolved_R          = Convolved_R         , $  ; Input keyword
;         Convolved_T          = Convolved_T         , $  ; Input keyword
;         f1                   = f1                  , $  ; Input keyword
;         f2                   = f2                  , $  ; Input keyword
;         Frequency            = Frequency           , $  ; Input keyword
;         Response             = Response                 ; Input keyword
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
;       Integral:              The integrated SRF value.
;                              UNITS:      N/A
;                              TYPE:       REAL
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Flags:                 Bit flags set/cleared during SRF processing.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       f0:                    The central frequency of the SRF.
;                              UNITS:      Inverse centimetres (cm^-1) or gigahertz (GHz)
;                              TYPE:       REAL
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Planck_Coeffs:         Vector of Planck function coefficients for the SRF.
;                              UNITS:      Variable
;                              TYPE:       REAL
;                              DIMENSION:  Rank-1
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Polychromatic_Coeffs:  Vector of polychromatic correction coefficient for the SRF.
;                              UNITS:      Variable
;                              TYPE:       REAL
;                              DIMENSION:  Rank-1
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
;       f1:                    The begin frequency of the SRF band.
;                              Used in conjunction with the Band keyword argument.
;                              UNITS:      Inverse centimetres (cm^-1) or gigahertz (GHz)
;                              TYPE:       REAL
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       f2:                    The vector of SRF band end frequencies.
;                              Used in conjunction with the Band keyword argument.
;                              UNITS:      Inverse centimetres (cm^-1) or gigahertz (GHz)
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
;         IDL> x->Set_Property, Sensor_Id=sid, Channel=ch
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 20-Apr-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO OSRF::Set_Property, $
  Band                                       , $  ; Optional input
  Debug                = Debug               , $  ; Input keyword
  Version              = Version             , $  ; Input keyword
  Sensor_Id            = Sensor_Id           , $  ; Input keyword
  WMO_Satellite_Id     = WMO_Satellite_Id    , $  ; Input keyword
  WMO_Sensor_Id        = WMO_Sensor_Id       , $  ; Input keyword
  Sensor_Type          = Sensor_Type         , $  ; Input keyword
  Channel              = Channel             , $  ; Input keyword
  Integral             = Integral            , $  ; Input keyword
  Flags                = Flags               , $  ; Input keyword
  f0                   = f0                  , $  ; Input keyword
  Planck_Coeffs        = Planck_Coeffs       , $  ; Input keyword
  Polychromatic_Coeffs = Polychromatic_Coeffs, $  ; Input keyword
  Convolved_R          = Convolved_R         , $  ; Input keyword
  Convolved_T          = Convolved_T         , $  ; Input keyword
  f1                   = f1                  , $  ; Input keyword
  f2                   = f2                  , $  ; Input keyword
  Frequency            = Frequency           , $  ; Input keyword
  Response             = Response                 ; Input keyword


  ; Set up
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler
 
  
  ; Check band argument
  _Band = 0L  ; Default
  IF ( N_ELEMENTS(Band) GT 0 ) THEN BEGIN
    _Band = LONG(Band[0]) - 1
    IF ( _Band LT 0 OR _Band GT self.n_Bands-1 ) THEN $
      MESSAGE, 'Invalid band specified.', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ENDIF
  
  
  ; Set scalar data
  IF ( N_ELEMENTS(Version             ) GT 0 ) THEN self.Version          = Version         
  IF ( N_ELEMENTS(Sensor_Id           ) GT 0 ) THEN self.Sensor_Id        = Sensor_Id       
  IF ( N_ELEMENTS(WMO_Satellite_ID    ) GT 0 ) THEN self.WMO_Satellite_ID = WMO_Satellite_ID
  IF ( N_ELEMENTS(WMO_Sensor_ID       ) GT 0 ) THEN self.WMO_Sensor_ID    = WMO_Sensor_ID   
  IF ( N_ELEMENTS(Sensor_Type         ) GT 0 ) THEN self.Sensor_Type      = Sensor_Type     
  IF ( N_ELEMENTS(Channel             ) GT 0 ) THEN self.Channel          = Channel         
  IF ( N_ELEMENTS(Integral            ) GT 0 ) THEN self.Integral             = Integral            
  IF ( N_ELEMENTS(Flags               ) GT 0 ) THEN self.Flags                = Flags               
  IF ( N_ELEMENTS(f0                  ) GT 0 ) THEN self.f0                   = f0                  
  IF ( N_ELEMENTS(Planck_Coeffs       ) GT 0 ) THEN self.Planck_Coeffs        = Planck_Coeffs       
  IF ( N_ELEMENTS(Polychromatic_Coeffs) GT 0 ) THEN self.Polychromatic_Coeffs = Polychromatic_Coeffs
  IF ( N_ELEMENTS(Convolved_R         ) GT 0 ) THEN self.Convolved_R = Convolved_R
  IF ( N_ELEMENTS(Convolved_T         ) GT 0 ) THEN self.Convolved_T = Convolved_T


  ; Set band limit frequency data
  ; (in case it's specified separately)
  IF ( N_ELEMENTS(f1) GT 0 ) THEN (*self.f1)[_Band] = f1
  IF ( N_ELEMENTS(f2) GT 0 ) THEN (*self.f2)[_Band] = f2

    
  ; Set frequency data
  n_Points = N_ELEMENTS(Frequency)
  IF ( n_Points GT 0 ) THEN BEGIN
    ; ...Check if structure has been allocated
    IF ( NOT self->Associated(Debug=Debug) ) THEN $
      MESSAGE, 'OSRF structure has not been allocated.', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    ; ...Check point numbering is consistent
    IF ( n_Points NE (*self.n_Points)[_Band] ) THEN $
      MESSAGE, 'Input frequency array different size from that allocated for band ' + $
               STRTRIM(_Band+1,2), $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    ; ...Assign frequency data
    (*self.f1)[_Band]         = Frequency[0]
    (*self.f2)[_Band]         = Frequency[n_Points-1]
    *(*self.Frequency)[_Band] = Frequency
  ENDIF


  ; Set response data
  n_Points = N_ELEMENTS(Response)
  IF ( n_Points GT 0 ) THEN BEGIN
    ; ...Check if structure has been allocated
    IF ( NOT self->Associated(Debug=Debug) ) THEN $
      MESSAGE, 'OSRF structure has not been allocated.', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    ; ...Check point numbering is consistent
    IF ( n_Points NE (*self.n_Points)[_Band] ) THEN $
      MESSAGE, 'Input response array different size from that allocated for band ' + $
               STRTRIM(_Band+1,2), $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    ; ...Assign response data
    *(*self.Response)[_Band] = Response
  ENDIF
  
  
  ; Done
  CATCH, /CANCEL
 
END ; PRO OSRF::Set_Property
