;+
; NAME:
;       OSRF::Set
;
; PURPOSE:
;       The OSRF::Set function method  assigns values to
;       OSRF components.
;
; CALLING SEQUENCE:
;       Result = Obj->[OSRF::]Set( $
;                  Debug           =Debug           , $  ; Input keyword
;                  Version         =Version         , $  ; Input keyword
;                  Sensor_Id       =Sensor_Id       , $  ; Input keyword
;                  WMO_Satellite_ID=WMO_Satellite_ID, $  ; Input keyword
;                  WMO_Sensor_ID   =WMO_Sensor_ID   , $  ; Input keyword
;                  Sensor_Type     =Sensor_Type     , $  ; Input keyword
;                  Channel         =Channel         , $  ; Input keyword
;                  Frequency       =Frequency       , $  ; Input keyword
;                  Response        =Response        , $  ; Input keyword
;                  Band            =Band              )  ; Input keyword
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
;       Version:            The version number of the SRF data.
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Sensor_ID:          A character string identifying the sensor and
;                           satellite platform used to contruct filenames.
;                           UNITS:      N/A
;                           TYPE:       CHARACTER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       WMO_Satellite_ID:   The WMO code used to identify satellite platforms.
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       WMO_Sensor_ID:      The WMO code used to identify sensors.
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(IN), OPTIONAL
;       Sensor_Type:        The flag indicating the type of sensor (IR, MW, etc)
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Channel:            The sensor channel for the currenobject.
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(IN), OPTIONAL
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
;       Band:               The 1-based band number for which the frequency and
;                           response refer to. If not specified, default value is 1.
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  SCALAR
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
;       Given a valid, allocated, OSRF object, x, assign various OSRF
;       components values like so,
;
;         IDL> Result = x->Set(Sensor_Id=sid, Channel=ch)
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 20-Apr-2009
;                       paul.vandelst@noaa.gov
;
;-

FUNCTION OSRF::Set, $
  Debug           =Debug           , $  ; Input keyword
  Version         =Version         , $  ; Input keyword
  Sensor_Id       =Sensor_Id       , $  ; Input keyword
  WMO_Satellite_ID=WMO_Satellite_ID, $  ; Input keyword
  WMO_Sensor_ID   =WMO_Sensor_ID   , $  ; Input keyword
  Sensor_Type     =Sensor_Type     , $  ; Input keyword
  Channel         =Channel         , $  ; Input keyword
  Frequency       =Frequency       , $  ; Input keyword
  Response        =Response        , $  ; Input keyword
  Band            =Band                 ; Input keyword


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
  
  
  ; Set scalar data
  IF ( N_ELEMENTS(Version         ) GT 0 ) THEN self.Version          = Version         
  IF ( N_ELEMENTS(Sensor_Id       ) GT 0 ) THEN self.Sensor_Id        = Sensor_Id       
  IF ( N_ELEMENTS(WMO_Satellite_ID) GT 0 ) THEN self.WMO_Satellite_ID = WMO_Satellite_ID
  IF ( N_ELEMENTS(WMO_Sensor_ID   ) GT 0 ) THEN self.WMO_Sensor_ID    = WMO_Sensor_ID   
  IF ( N_ELEMENTS(Sensor_Type     ) GT 0 ) THEN self.Sensor_Type      = Sensor_Type     
  IF ( N_ELEMENTS(Channel         ) GT 0 ) THEN self.Channel          = Channel         

  
  ; Set frequency data
  n_Points = N_ELEMENTS(Frequency)
  IF ( n_Points GT 0 ) THEN BEGIN
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
  RETURN, SUCCESS
 
END ; FUNCTION OSRF::Set
