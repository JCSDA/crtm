;+
; NAME:
;       SensorInfo::Assign
;
; PURPOSE:
;       The SensorInfo::Assign function method copies a valid SensorInfo
;       object.
;
; CALLING SEQUENCE:
;       Result = Obj->[SensorInfo::]Assign( new,        $  ; Output
;                                           Debug=Debug )  ; Input keyword
;
; OUTPUT:
;       new:         A deep copy of the SensorInfo object.
;                    UNITS:      N/A
;                    TYPE:       SensorInfo object
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN)
;                    
; INPUT KEYWORD PARAMETERS:
;       Debug:       Set this keyword for debugging.
;                    If NOT SET => Error handler is enabled. (DEFAULT)
;                       SET     => Error handler is disabled; Routine
;                                  traceback output is enabled.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
;;
; FUNCTION RESULT:
;       Result:      The return value is an integer defining the error
;                    status. The error codes are defined in the error_codes
;                    include file.
;                    If == SUCCESS the computation was sucessful
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
;       Given an instance of a SensorInfo object,
;
;         IDL> HELP, x
;         X               OBJREF    = <ObjHeapVar8(SENSORINFO)>
;
;       a new instance of the data object is created by:
;
;         IDL> Result = x->Assign(y)
;         IDL> help, y
;         Y               OBJREF    = <ObjHeapVar12(SENSORINFO)>
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 01-Oct-2008
;                       paul.vandelst@noaa.gov
;
;-

FUNCTION SensorInfo::Assign, new, $       ; Output
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

  ; ALL *input* pointers must be associated
  IF ( self->Associated(Debug=Debug) EQ FALSE ) THEN $
    MESSAGE, 'Some or all INPUT SensorInfo pointer members are NOT associated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch

  ; Destroy input object if defined.
  IF ( N_ELEMENTS(new) GT 0 ) THEN $
    IF ( SIZE(new, /TNAME) EQ 'OBJREF' ) THEN OBJ_DESTROY, new, Debug=Debug
  
  ; Create a new object reference
  new = OBJ_NEW('SensorInfo',Debug=Debug)


  ; Allocate the pointer members
  ; ----------------------------
  Result = new->Allocate( self.n_Channels,Debug=Debug )
  IF ( Result NE SUCCESS ) THEN $
    MESSAGE, 'Error allocating output SensorInfo structure', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Assign data components
  ; ----------------------
  new.Sensor_Name      = self.Sensor_Name     
  new.Satellite_Name   = self.Satellite_Name  
  new.Sensor_ID        = self.Sensor_ID  
  new.WMO_Satellite_Id = self.WMO_Satellite_Id
  new.WMO_Sensor_Id    = self.WMO_Sensor_Id
  new.Microwave_Flag   = self.Microwave_Flag
  new.Sensor_Type      = self.Sensor_Type   
  *new.Sensor_Channel  = *self.Sensor_Channel
  *new.Use_Flag        = *self.Use_Flag      
  *new.Noise           = *self.Noise          

  ; Done
  CATCH, /CANCEL
  RETURN, SUCCESS

END ; FUNCTION SensorInfo::Assign
