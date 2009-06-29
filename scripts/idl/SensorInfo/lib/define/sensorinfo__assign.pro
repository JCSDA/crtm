;+
; NAME:
;       SensorInfo::Assign
;
; PURPOSE:
;       The SensorInfo::Assign procedure method copies a valid SensorInfo
;       object.
;
; CALLING SEQUENCE:
;       Obj->[SensorInfo::]Assign, $
;         new        , $  ; Output
;         Debug=Debug     ; Input keyword
;
; ARGUMENTS:
;       new:         A deep copy of the SensorInfo object.
;                    UNITS:      N/A
;                    TYPE:       SensorInfo object
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN)
;                    
; KEYWORDS:
;       Debug:       Set this keyword for debugging.
;                    If NOT SET => Error handler is enabled. (DEFAULT)
;                       SET     => Error handler is disabled; Routine
;                                  traceback output is enabled.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
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
;         IDL> x->Assign,y
;         IDL> help, y
;         Y               OBJREF    = <ObjHeapVar12(SENSORINFO)>
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 01-Oct-2008
;                       paul.vandelst@noaa.gov
;
;-

PRO SensorInfo::Assign, $
  new, $       ; Output
  Debug=Debug  ; Input keyword

  ; Set up
  ; ...SensorInfo parameters
  @sensorinfo_parameters
  ; ...Set up error handler
  @sensorinfo_pro_err_handler
  ; ...ALL *input* pointers must be associated
  IF ( self->Associated(Debug=Debug) EQ FALSE ) THEN $
    MESSAGE, 'Some or all input SensorInfo pointer members are NOT associated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ; ...Destroy output object if defined.
  IF ( N_ELEMENTS(new) GT 0 ) THEN $
    IF ( SIZE(new, /TNAME) EQ 'OBJREF' ) THEN OBJ_DESTROY, new, Debug=Debug
  ; ...Create a new object reference
  new = OBJ_NEW('SensorInfo',Debug=Debug)


  ; Allocate the pointer members
  new->Allocate, self.n_Channels, Debug=Debug


  ; Assign data components
  new.Sensor_Name      = self.Sensor_Name     
  new.Satellite_Name   = self.Satellite_Name  
  new.Sensor_ID        = self.Sensor_ID  
  new.WMO_Satellite_Id = self.WMO_Satellite_Id
  new.WMO_Sensor_Id    = self.WMO_Sensor_Id
  new.Sensor_Type      = self.Sensor_Type   
  *new.Sensor_Channel  = *self.Sensor_Channel
  *new.Use_Flag        = *self.Use_Flag      
  *new.Noise           = *self.Noise          


  ; Done
  CATCH, /CANCEL

END ; PRO SensorInfo::Assign
