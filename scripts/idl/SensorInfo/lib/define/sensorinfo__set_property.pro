;+
; NAME:
;       SensorInfo::Set_Property
;
; PURPOSE:
;       The SensorInfo::Set_Property procedure method sets the value of a property
;       or group of properties for this object.
;
; CALLING SEQUENCE:
;       Obj->[SensorInfo::]Set_Property, $
;         Debug            = Debug           , $  ; Input keyword
;         Sensor_Name      = Sensor_Name     , $  ; Input keyword
;         Satellite_Name   = Satellite_Name  , $  ; Input keyword
;         Sensor_Id        = Sensor_Id       , $  ; Input keyword
;         WMO_Satellite_ID = WMO_Satellite_ID, $  ; Input keyword
;         WMO_Sensor_ID    = WMO_Sensor_ID   , $  ; Input keyword
;         Sensor_Type      = Sensor_Type     , $  ; Input keyword
;         Sensor_Channel   = Sensor_Channel  , $  ; Input keyword
;         Use_Flag         = Use_Flag        , $  ; Input keyword
;         Noise            = Noise                ; Input keyword
;
; KEYWORDS:
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
;       sensorinfo_pro_err_handler: Error handler code for SensorInfo procedures.
;
; EXAMPLE:
;       Given a valid, allocated object, x, assign various property
;       values like so,
;
;         IDL> x->Set_Property, Sensor_Id=sid
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 01-Oct-2008
;                       paul.vandelst@noaa.gov
;
;-

PRO SensorInfo::Set_Property, $
  Debug           =Debug           , $  ; Input keyword
  Sensor_Name     =Sensor_Name     , $  ; Input keyword 
  Satellite_Name  =Satellite_Name  , $  ; Input keyword 
  Sensor_Id       =Sensor_Id       , $  ; Input keyword 
  WMO_Satellite_ID=WMO_Satellite_ID, $  ; Input keyword 
  WMO_Sensor_ID   =WMO_Sensor_ID   , $  ; Input keyword 
  Sensor_Type     =Sensor_Type     , $  ; Input keyword 
  Sensor_Channel  =Sensor_Channel  , $  ; Input keyword 
  Use_Flag        =Use_Flag        , $  ; Input keyword 
  Noise           =Noise                ; Input keyword 

  ; Set up
  ; ...Parameters
  @sensorinfo_parameters
  ; ...Set up error handler
  @sensorinfo_pro_err_handler

  
  ; Check if structure has been allocated
  IF ( NOT self->Associated(Debug=Debug) OR self.n_Channels EQ 0 ) THEN $
    MESSAGE, 'SensorInfo object has not been allocated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  

  ; Set data
  IF ( N_ELEMENTS(Sensor_Name     ) GT 0 ) THEN self.Sensor_Name      = Sensor_Name     
  IF ( N_ELEMENTS(Satellite_Name  ) GT 0 ) THEN self.Satellite_Name   = Satellite_Name  
  IF ( N_ELEMENTS(Sensor_Id       ) GT 0 ) THEN self.Sensor_Id        = Sensor_Id       
  IF ( N_ELEMENTS(WMO_Satellite_ID) GT 0 ) THEN self.WMO_Satellite_ID = WMO_Satellite_ID
  IF ( N_ELEMENTS(WMO_Sensor_ID   ) GT 0 ) THEN self.WMO_Sensor_ID    = WMO_Sensor_ID   
  IF ( N_ELEMENTS(Sensor_Type     ) GT 0 ) THEN self.Sensor_Type      = Sensor_Type
  IF ( N_ELEMENTS(Sensor_Channel  ) GT 0 ) THEN *self.Sensor_Channel  = Sensor_Channel
  IF ( N_ELEMENTS(Use_Flag        ) GT 0 ) THEN *self.Use_Flag        = Use_Flag
  IF ( N_ELEMENTS(Noise           ) GT 0 ) THEN *self.Noise           = Noise
  
  
  ; Done
  CATCH, /CANCEL
 
END ; PRO SensorInfo::Set_Property
