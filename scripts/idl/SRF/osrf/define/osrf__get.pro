;+
; NAME:
;       OSRF::Get
;
; PURPOSE:
;       The OSRF::Get function method retrieves the value of an
;       OSRF component.
;
; CALLING SEQUENCE:
;       Result = Obj->[OSRF::]Get( $
;                  Debug            = Debug           , $  ; Input keyword
;                  Band             = Band            , $  ; Input keyword
;                  n_Bands          = n_Bands         , $  ; Output keyword
;                  Version          = Version         , $  ; Output keyword
;                  Sensor_Id        = Sensor_Id       , $  ; Output keyword
;                  WMO_Satellite_ID = WMO_Satellite_ID, $  ; Output keyword
;                  WMO_Sensor_ID    = WMO_Sensor_ID   , $  ; Output keyword
;                  Sensor_Type      = Sensor_Type     , $  ; Output keyword
;                  Channel          = Channel         , $  ; Output keyword
;                  Integral         = Integral        , $  ; Output keyword
;                  f1               = f1              , $  ; Output keyword
;                  f2               = f2              , $  ; Output keyword
;                  n_Points         = n_Points        , $  ; Output keyword
;                  Frequency        = Frequency       , $  ; Output keyword
;                  Response         = Response          )  ; Output keyword
;
; INPUT KEYWORD PARAMETERS:
;       Debug:              Set this keyword for debugging.
;                           If NOT SET => Error handler is enabled. (DEFAULT)
;                              SET     => Error handler is disabled; Routine
;                                         traceback output is enabled.
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Band:               The 1-based band number for which the frequency and
;                           response refer to. If not specified, default value is 1.
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  SCALAR
;                           ATTRIBUTES: INTENT(IN), OPTIONAL
;
; OUTPUT KEYWORD PARAMETERS:
;       n_Bands:            The number of bands dimension of the SRF.
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  SCALAR
;                           ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Version:            The version number of the SRF data.
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Sensor_ID:          A character string identifying the sensor and
;                           satellite platform used to contruct filenames.
;                           UNITS:      N/A
;                           TYPE:       CHARACTER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       WMO_Satellite_ID:   The WMO code used to identify satellite platforms.
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       WMO_Sensor_ID:      The WMO code used to identify sensors.
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(OUT), OPTIONAL
;       Sensor_Type:        The flag indicating the type of sensor (IR, MW, etc)
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Channel:            The sensor channel for the currenobject.
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Integral:           The integrated SRF value.
;                           UNITS:      N/A
;                           TYPE:       REAL
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       f1:                 The begin frequency of the SRF band.
;                           Used in conjunction with the Band keyword argument.
;                           UNITS:      Inverse centimetres (cm^-1)
;                           TYPE:       REAL
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       f2:                 The vector of SRF band end frequencies.
;                           Used in conjunction with the Band keyword argument.
;                           UNITS:      Inverse centimetres (cm^-1)
;                           TYPE:       REAL
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       n_Points:           The number of points used to represent a band of
;                           the SRF.
;                           Used in conjunction with the Band keyword argument.
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Frequency:          The frequency grid for an SRF band.
;                           Used in conjunction with the Band keyword argument.
;                           UNITS:      Inverse centimetres (cm^-1)
;                           TYPE:       REAL
;                           DIMENSION:  n_Points
;                           ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Response:           The response data for an SRF band.
;                           Used in conjunction with the Band keyword argument.
;                           UNITS:      N/A
;                           TYPE:       REAL
;                           DIMENSION:  n_Points
;                           ATTRIBUTES: INTENT(IN), OPTIONAL
;
; FUNCTION RESULT:
;       Result:      The return value is an integer defining the error
;                    status. The error codes are defined in the error_codes
;                    include file.
;                    If == SUCCESS the component retrieval was successful
;                       == FAILURE an unrecoverable error occurred
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;
; INCLUDE FILES:
;       srf_parameters: Include file containing SRF specific
;                       parameter value definitions.
;
;       osrf_func_err_handler: Error handler code for OSRF functions.
;
; EXAMPLE:
;       Given a valid OSRF object, x, obtain various OSRF
;       components values like so,
;
;         IDL> Result = x->Get(Sensor_Id=sid, Channel=ch)
;         IDL> HELP, sid, ch
;         SID             STRING    = 'hirs4_n18           '
;         CH              LONG      =            7
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 22-May-2009
;                       paul.vandelst@noaa.gov
;
;-

FUNCTION OSRF::Get, $
  Debug            = Debug           , $  ; Input keyword
  Band             = Band            , $  ; Input keyword
  n_Bands          = n_Bands         , $  ; Output keyword
  Version          = Version         , $  ; Output keyword
  Sensor_Id        = Sensor_Id       , $  ; Output keyword
  WMO_Satellite_ID = WMO_Satellite_ID, $  ; Output keyword
  WMO_Sensor_ID    = WMO_Sensor_ID   , $  ; Output keyword
  Sensor_Type      = Sensor_Type     , $  ; Output keyword
  Channel          = Channel         , $  ; Output keyword
  Integral         = Integral        , $  ; Output keyword
  f1               = f1              , $  ; Output keyword
  f2               = f2              , $  ; Output keyword
  n_Points         = n_Points        , $  ; Output keyword
  Frequency        = Frequency       , $  ; Output keyword
  Response         = Response             ; Output keyword


  ; Set up
  ; ...Generic SRF parameters
  @srf_parameters
  
  ; ...Set up error handler
  @osrf_func_err_handler
 
  
  ; Check if structure has been allocated
  IF ( self->Associated(Debug=Debug) EQ FALSE ) THEN $
    MESSAGE, 'OSRF structure has not been allocated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch

  ; Check band argument
  _Band = 0L  ; Default
  IF ( N_ELEMENTS(Band) GT 0 ) THEN BEGIN
    _Band = LONG(Band[0]) - 1
    IF ( _Band LT 0 OR _Band GT self.n_Bands-1 ) THEN $
      MESSAGE, 'Invalid band specified.', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ENDIF


  ; Get data
  IF ( ARG_PRESENT(n_Bands         ) ) THEN n_Bands          = self.n_Bands
  IF ( ARG_PRESENT(Version         ) ) THEN Version          = self.Version         
  IF ( ARG_PRESENT(Sensor_Id       ) ) THEN Sensor_Id        = self.Sensor_Id       
  IF ( ARG_PRESENT(WMO_Satellite_ID) ) THEN WMO_Satellite_ID = self.WMO_Satellite_ID
  IF ( ARG_PRESENT(WMO_Sensor_ID   ) ) THEN WMO_Sensor_ID    = self.WMO_Sensor_ID   
  IF ( ARG_PRESENT(Sensor_Type     ) ) THEN Sensor_Type      = self.Sensor_Type     
  IF ( ARG_PRESENT(Channel         ) ) THEN Channel          = self.Channel         
  IF ( ARG_PRESENT(Integral        ) ) THEN Integral         = self.Integral
  IF ( ARG_PRESENT(f1              ) ) THEN f1               = (*self.f1)[_Band]              
  IF ( ARG_PRESENT(f2              ) ) THEN f2               = (*self.f2)[_Band]             
  IF ( ARG_PRESENT(n_Points        ) ) THEN n_Points         = (*self.n_Points)[_Band]
  IF ( ARG_PRESENT(Frequency       ) ) THEN Frequency        = *(*self.Frequency)[_Band]
  IF ( ARG_PRESENT(Response        ) ) THEN Response         = *(*self.Response)[_Band]

  
  ; Done
  CATCH, /CANCEL
  RETURN, SUCCESS
 
END ; FUNCTION OSRF::Get
