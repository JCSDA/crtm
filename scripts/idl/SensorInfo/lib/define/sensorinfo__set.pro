;+
; NAME:
;       SensorInfo::Set
;
; PURPOSE:
;       The SensorInfo::Set function method assigns values to
;       SensorInfo components.
;
; CALLING SEQUENCE:
;       Result = Obj->[SensorInfo::]Set( Debug           =Debug           , $  ; Input keyword
;                                        Sensor_Name     =Sensor_Name     , $  ; Input keyword
;                                        Satellite_Name  =Satellite_Name  , $  ; Input keyword
;                                        Sensor_Id       =Sensor_Id       , $  ; Input keyword
;                                        WMO_Satellite_ID=WMO_Satellite_ID, $  ; Input keyword
;                                        WMO_Sensor_ID   =WMO_Sensor_ID   , $  ; Input keyword
;                                        Microwave_Flag  =Microwave_Flag  , $  ; Input keyword
;                                        Sensor_Type     =Sensor_Type     , $  ; Input keyword
;                                        Sensor_Channel  =Sensor_Channel  , $  ; Input keyword
;                                        Use_Flag        =Use_Flag        , $  ; Input keyword
;                                        Noise           =Noise                ; Input keyword
;
; INPUT KEYWORD PARAMETERS:
;       Debug:            Set this keyword for debugging.
;                         If NOT SET => Error handler is enabled. (DEFAULT)
;                            SET     => Error handler is disabled; Routine
;                                       traceback output is enabled.
;                         UNITS:      N/A
;                         TYPE:       INTEGER
;                         DIMENSION:  Scalar
;                         ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Sensor_Name:      Set this keyword to a named variable to set the
;                         name of the sensor.
;                         UNITS:      N/A
;                         TYPE:       CHARACTER
;                         DIMENSION:  Scalar
;                         ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Satellite_Name:   Set this keyword to a named variable to set the
;                         name of the satellite platform.
;                         UNITS:      N/A
;                         TYPE:       CHARACTER
;                         DIMENSION:  Scalar
;                         ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Sensor_Id:        Set this keyword to a named variable to set the
;                         sensor Id.
;                         UNITS:      N/A
;                         TYPE:       CHARACTER
;                         DIMENSION:  Scalar
;                         ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       WMO_Satellite_Id: Set this keyword to a named variable to set the
;                         WMO satellite Id assigned to the satellite platform.
;                         A value of 1023 is used to indicate a missing, or
;                         unassigned, id.
;                         UNITS:      N/A
;                         TYPE:       INTEGER
;                         DIMENSION:  Scalar
;                         ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       WMO_Sensor_Id:    Set this keyword to a named variable to set the
;                         WMO sensor Id assigned to the sensor type. A value
;                         of 2047 is used to indicate a missing, or unassigned, id.
;                         UNITS:      N/A
;                         TYPE:       INTEGER
;                         DIMENSION:  Scalar
;                         ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Microwave_Flag:   **DEPRECATED. Use Sensor_Type**
;                         Set this keyword to a named variable to set the
;                         flag indicating if the current sensor is a microwave
;                         instrument.
;                         If 0 => Sensor is NOT a microwave instrument.
;                            1 => Sensor is a microwave instrument.
;                         UNITS:      N/A
;                         TYPE:       INTEGER
;                         DIMENSION:  Scalar
;                         ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Sensor_Type:      Set this keyword to a named variable to set the
;                         flag indicating the sensor type. Valid sensor type
;                         values are defined in the sensorinfo_parameters
;                         include file. Defined parameter values are
;                           MICROWAVE_SENSOR
;                           INFRARED_SENSOR   
;                           VISIBLE_SENSOR    
;                           ULTRAVIOLET_SENSOR
;                         UNITS:      N/A
;                         TYPE:       INTEGER
;                         DIMENSION:  Scalar
;                         ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Sensor_Channel:   Set this keyword to a named variable to set the
;                         array of sensor channel numbers.
;                         UNITS:      N/A
;                         TYPE:       INTEGER
;                         DIMENSION:  Rank-1
;                         ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Use_Flag:         Set this keyword to a named variable to set the
;                         array of channel use flags.
;                         If 0 => do not use this channel
;                            1 => channel can be used.
;                         Note that in the Master SensorInfo datafile, these
;                         are all set to 1 for all instruments, all channels.
;                         UNITS:      N/A
;                         TYPE:       INTEGER
;                         DIMENSION:  Rank-1
;                         ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Noise:            Set this keyword to a named variable to set the
;                         estimated channel noise.
;                         Note that in the Master SensorInfo datafile, many
;                         instrument channels have a default value of 100K.
;                         UNITS:      Kelvin (K)
;                         TYPE:       INTEGER
;                         DIMENSION:  Rank-1
;                         ATTRIBUTES: INTENT(IN), OPTIONAL
;
; FUNCTION RESULT:
;       Result:           The return value is an integer defining the error
;                         status. The error codes are defined in the error_codes
;                         include file.
;                         If == SUCCESS the component assignment was successful
;                            == FAILURE an unrecoverable error occurred
;                         UNITS:      N/A
;                         TYPE:       INTEGER
;                         DIMENSION:  Scalar
;
; INCLUDE FILES:
;       sensorinfo_parameters: Include file containing SensorInfo specific
;                              parameter value definitions.
;
;       error_codes:           Include file containing error code definitions.
;
; EXAMPLE:
;       Given a valid SensorInfo object, x, allocated for 5 channels,
;
;         IDL> x = OBJ_NEW('SensorInfo')
;         IDL> Result = x->Allocate(5)
;
;       the following SensorInfo properties
;
;         IDL> sid = 'sensor_platform'
;         IDL> ch  = [3,4,5,6,7]
;
;       are set via,
;
;         IDL> Result = x->Set(Sensor_Id=sid, Sensor_Channel=ch)
;
;       Inspect the result:
;
;         IDL> x->Inspect
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 01-Oct-2008
;                       paul.vandelst@noaa.gov
;
;-

FUNCTION SensorInfo::Set, Debug           =Debug           , $  ; Input keyword
                          Sensor_Name     =Sensor_Name     , $  ; Input keyword 
                          Satellite_Name  =Satellite_Name  , $  ; Input keyword 
                          Sensor_Id       =Sensor_Id       , $  ; Input keyword 
                          WMO_Satellite_ID=WMO_Satellite_ID, $  ; Input keyword 
                          WMO_Sensor_ID   =WMO_Sensor_ID   , $  ; Input keyword 
                          Microwave_Flag  =Microwave_Flag  , $  ; Input keyword 
                          Sensor_Type     =Sensor_Type     , $  ; Input keyword 
                          Sensor_Channel  =Sensor_Channel  , $  ; Input keyword 
                          Use_Flag        =Use_Flag        , $  ; Input keyword 
                          Noise           =Noise                ; Input keyword 

  ; Set up
  ; ------
  ; Include SensorInfo parameters
  @sensorinfo_parameters
  
  ; error handler
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
  
  ; Check if structure has been allocated
  IF ( self.n_Channels EQ 0 ) THEN $
    MESSAGE, 'n_Channels structure dimension is zero!', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch

  ; Add data
  ; --------
  ; Scalars
  IF ( N_ELEMENTS(Sensor_Name     ) GT 0 ) THEN self.Sensor_Name      = Sensor_Name     
  IF ( N_ELEMENTS(Satellite_Name  ) GT 0 ) THEN self.Satellite_Name   = Satellite_Name  
  IF ( N_ELEMENTS(Sensor_Id       ) GT 0 ) THEN self.Sensor_Id        = Sensor_Id       
  IF ( N_ELEMENTS(WMO_Satellite_ID) GT 0 ) THEN self.WMO_Satellite_ID = WMO_Satellite_ID
  IF ( N_ELEMENTS(WMO_Sensor_ID   ) GT 0 ) THEN self.WMO_Sensor_ID    = WMO_Sensor_ID   
  IF ( N_ELEMENTS(Microwave_Flag  ) GT 0 ) THEN self.Microwave_Flag   = Microwave_Flag  
  IF ( N_ELEMENTS(Sensor_Type     ) GT 0 ) THEN self.Sensor_Type      = Sensor_Type
  
  ; Arrays
  IF ( N_ELEMENTS(Sensor_Channel) GT 0 ) THEN BEGIN
    IF ( N_ELEMENTS(Sensor_Channel) EQ self.n_Channels ) THEN $
      *self.Sensor_Channel = Sensor_Channel $
    ELSE $
      MESSAGE, 'Size of Sensor_Channel input inconsistent with n_Channels dimension.', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ENDIF
             
  IF ( N_ELEMENTS(Use_Flag) GT 0 ) THEN BEGIN
    IF ( N_ELEMENTS(Use_Flag) EQ self.n_Channels ) THEN $
      *self.Use_Flag = Use_Flag $
    ELSE $
      MESSAGE, 'Size of Use_Flag input inconsistent with n_Channels dimension.', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ENDIF
             
  IF ( N_ELEMENTS(Noise) GT 0 ) THEN BEGIN
    IF ( N_ELEMENTS(Noise) EQ self.n_Channels ) THEN $
      *self.Noise = Noise $
    ELSE $
      MESSAGE, 'Size of Noise input inconsistent with n_Channels dimension.', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ENDIF
  
  ; Done
  CATCH, /CANCEL
  RETURN, SUCCESS
 
END ; FUNCTION SensorInfo::Set
