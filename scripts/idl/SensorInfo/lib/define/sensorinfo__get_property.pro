;+
; NAME:
;       SensorInfo::Get_Property
;
; PURPOSE:
;       The SensorInfo::Get_Property procedure method retrieves the value of a property
;       or group of properties for this object.
;
; CALLING SEQUENCE:
;       Obj->[SensorInfo::]Get_Property, $
;         Debug            = Debug           , $  ; Input keyword
;         Sensor_Name      = Sensor_Name     , $  ; Output keyword
;         Satellite_Name   = Satellite_Name  , $  ; Output keyword
;         Sensor_Id        = Sensor_Id       , $  ; Output keyword
;         WMO_Satellite_ID = WMO_Satellite_ID, $  ; Output keyword
;         WMO_Sensor_ID    = WMO_Sensor_ID   , $  ; Output keyword
;         Sensor_Type      = Sensor_Type     , $  ; Output keyword
;         Sensor_Channel   = Sensor_Channel  , $  ; Output keyword
;         Use_Flag         = Use_Flag        , $  ; Output keyword
;         Noise            = Noise           , $  ; Output keyword
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
;       Sensor_Name:      Set this keyword to a named variable to return the
;                         name of the sensor.
;                         UNITS:      N/A
;                         TYPE:       CHARACTER
;                         DIMENSION:  Scalar
;                         ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Satellite_Name:   Set this keyword to a named variable to return the
;                         name of the satellite platform.
;                         UNITS:      N/A
;                         TYPE:       CHARACTER
;                         DIMENSION:  Scalar
;                         ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Sensor_Id:        Set this keyword to a named variable to return the
;                         sensor Id.
;                         UNITS:      N/A
;                         TYPE:       CHARACTER
;                         DIMENSION:  Scalar
;                         ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       WMO_Satellite_Id: Set this keyword to a named variable to return the
;                         WMO satellite Id assigned to the satellite platform.
;                         A value of 1023 indicates a missing, or unassigned, id.
;                         UNITS:      N/A
;                         TYPE:       INTEGER
;                         DIMENSION:  Scalar
;                         ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       WMO_Sensor_Id:    Set this keyword to a named variable to return the
;                         WMO sensor Id assigned to the sensor type. A value
;                         of 2047 indicates a missing, or unassigned, id.
;                         UNITS:      N/A
;                         TYPE:       INTEGER
;                         DIMENSION:  Scalar
;                         ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Sensor_Type:      Set this keyword to a named variable to return the
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
;                         ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Sensor_Channel:   Set this keyword to a named variable to return the
;                         array of sensor channel numbers.
;                         UNITS:      N/A
;                         TYPE:       INTEGER
;                         DIMENSION:  Rank-1
;                         ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Use_Flag:         Set this keyword to a named variable to return the
;                         array of channel use flags.
;                         If 0 => do not use this channel
;                            1 => channel can be used.
;                         Note that in the Master SensorInfo datafile, these
;                         are all set to 1 for all istruments, all channels.
;                         UNITS:      N/A
;                         TYPE:       INTEGER
;                         DIMENSION:  Rank-1
;                         ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Noise:            Set this keyword to a named variable to return the
;                         estimated channel noise.
;                         Note that in the Master SensorInfo datafile, many
;                         instrument channels have a default value of 100K.
;                         UNITS:      Kelvin (K)
;                         TYPE:       INTEGER
;                         DIMENSION:  Rank-1
;                         ATTRIBUTES: INTENT(OUT), OPTIONAL
;
; INCLUDE FILES:
;       sensorinfo_parameters: Include file containing SensorInfo specific
;                              parameter value definitions.
;
;       sensorinfo_pro_err_handler: Error handler code for SensorInfo procedures.
;
; EXAMPLE:
;       Given a valid SensorInfo object, x, obtain various SensorInfo
;       components values like so,
;
;         IDL> x->Get_Property, Sensor_Id=sid, Sensor_Channel=ch
;         IDL> HELP, sid, ch
;         SID             STRING    = 'hirs4_n18           '
;         CH              LONG      = Array[19]
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 01-Oct-2008
;                       paul.vandelst@noaa.gov
;
;-

PRO SensorInfo::Get_Property, $
  Debug            = Debug           , $  ; Input keyword
  Sensor_Name      = Sensor_Name     , $  ; Output keyword 
  Satellite_Name   = Satellite_Name  , $  ; Output keyword 
  Sensor_Id        = Sensor_Id       , $  ; Output keyword 
  WMO_Satellite_ID = WMO_Satellite_ID, $  ; Output keyword 
  WMO_Sensor_ID    = WMO_Sensor_ID   , $  ; Output keyword 
  Sensor_Type      = Sensor_Type     , $  ; Output keyword 
  Sensor_Channel   = Sensor_Channel  , $  ; Output keyword 
  Use_Flag         = Use_Flag        , $  ; Output keyword 
  Noise            = Noise                ; Output keyword 

  ; Set up
  ; ...Parameters
  @sensorinfo_parameters
  ; ...Set up error handler
  @sensorinfo_pro_err_handler

  
  ; Check if structure has been allocated
  IF ( NOT self->Associated(Debug=Debug) OR self.n_Channels EQ 0 ) THEN $
    MESSAGE, 'SensorInfo object has not been allocated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Get data
  IF ( ARG_PRESENT( Sensor_Name      ) ) THEN Sensor_Name      = self.Sensor_Name     
  IF ( ARG_PRESENT( Satellite_Name   ) ) THEN Satellite_Name   = self.Satellite_Name  
  IF ( ARG_PRESENT( Sensor_Id        ) ) THEN Sensor_Id        = self.Sensor_Id       
  IF ( ARG_PRESENT( WMO_Satellite_ID ) ) THEN WMO_Satellite_ID = self.WMO_Satellite_ID
  IF ( ARG_PRESENT( WMO_Sensor_ID    ) ) THEN WMO_Sensor_ID    = self.WMO_Sensor_ID   
  IF ( ARG_PRESENT( Microwave_Flag   ) ) THEN Microwave_Flag   = self.Microwave_Flag  
  IF ( ARG_PRESENT( Sensor_Type      ) ) THEN Sensor_Type      = self.Sensor_Type     
  IF ( ARG_PRESENT( Sensor_Channel   ) ) THEN Sensor_Channel   = *self.Sensor_Channel
  IF ( ARG_PRESENT( Use_Flag         ) ) THEN Use_Flag         = *self.Use_Flag      
  IF ( ARG_PRESENT( Noise            ) ) THEN Noise            = *self.Noise         
  
  
  ; Done
  CATCH, /CANCEL
 
END ; PRO SensorInfo::Get_Property
