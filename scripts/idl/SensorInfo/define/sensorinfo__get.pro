;+
; NAME:
;       SensorInfo::Get
;
; PURPOSE:
;       The SensorInfo::Get function method retrieves the value of a
;       SensorInfo component.
;
; CALLING SEQUENCE:
;       Result = Obj->[SensorInfo::]Get( Debug           =Debug           , $  ; Input keyword
;                                        Sensor_Name     =Sensor_Name     , $  ; Output keyword
;                                        Satellite_Name  =Satellite_Name  , $  ; Output keyword
;                                        Sensor_Id       =Sensor_Id       , $  ; Output keyword
;                                        WMO_Satellite_ID=WMO_Satellite_ID, $  ; Output keyword
;                                        WMO_Sensor_ID   =WMO_Sensor_ID   , $  ; Output keyword
;                                        Microwave_Flag  =Microwave_Flag  , $  ; Output keyword
;                                        Sensor_Type     =Sensor_Type     , $  ; Output keyword
;                                        Sensor_Channel  =Sensor_Channel  , $  ; Output keyword
;                                        Use_Flag        =Use_Flag        , $  ; Output keyword
;                                        Noise           =Noise           , $  ; Output keyword
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
; OUTPUT KEYWORD PARAMETERS:
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
;       Microwave_Flag:   **DEPRECATED. Use Sensor_Type**
;                         Set this keyword to a named variable to return the
;                         flag indicating if the current sensor is a microwave
;                         instrument.
;                         If 0 => Sensor is NOT a microwave instrument.
;                            1 => Sensor is a microwave instrument.
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
;       sensorinfo_parameters: Include file containing SensorInfo specific
;                              parameter value definitions.
;
;       error_codes:           Include file containing error code definitions.
;
; EXAMPLE:
;       Given a valid SensorInfo object, x, obtain various SensorInfo
;       components values like so,
;
;         IDL> Result = x->Get(Sensor_Id=sid, Sensor_Channel=ch)
;         IDL> HELP, sid, ch
;         SID             STRING    = 'hirs4_n18           '
;         CH              LONG      = Array[19]
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 01-Oct-2008
;                       paul.vandelst@noaa.gov
;
;-

FUNCTION SensorInfo::Get, Sensor_Name     =Sensor_Name     , $  ; Output keyword 
                          Satellite_Name  =Satellite_Name  , $  ; Output keyword 
                          Sensor_Id       =Sensor_Id       , $  ; Output keyword 
                          WMO_Satellite_ID=WMO_Satellite_ID, $  ; Output keyword 
                          WMO_Sensor_ID   =WMO_Sensor_ID   , $  ; Output keyword 
                          Microwave_Flag  =Microwave_Flag  , $  ; Output keyword 
                          Sensor_Type     =Sensor_Type     , $  ; Output keyword 
                          Sensor_Channel  =Sensor_Channel  , $  ; Output keyword 
                          Use_Flag        =Use_Flag        , $  ; Output keyword 
                          Noise           =Noise           , $  ; Output keyword 
                          Debug=Debug  ; Input keyword

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

  ; Get data
  ; --------
  ; Scalars
  Sensor_Name      = self.Sensor_Name     
  Satellite_Name   = self.Satellite_Name  
  Sensor_Id        = self.Sensor_Id       
  WMO_Satellite_ID = self.WMO_Satellite_ID
  WMO_Sensor_ID    = self.WMO_Sensor_ID   
  Microwave_Flag   = self.Microwave_Flag  
  Sensor_Type      = self.Sensor_Type     
  
  ; Arrays
  Sensor_Channel = *self.Sensor_Channel
  Use_Flag       = *self.Use_Flag      
  Noise          = *self.Noise         
  
  ; Done
  CATCH, /CANCEL
  RETURN, SUCCESS
 
END ; FUNCTION SensorInfo::Get
