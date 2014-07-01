;+
; NAME:
;       SensorInfo::Init
;
; PURPOSE:
;       The SensorInfo::Init function method initialises a SensorInfo
;       object.
;
;       NOTE: Init methods are special *lifecycle methods* and, as
;             such, cannot be called outside the context of object
;             creation. This means that in most cases you cannot call
;             the Init method directly. There is one exception to this
;             rule: if you write your own subclass of this class, you
;             can call the Init method from within the Init method of
;             the subclass.
;
; CALLING SEQUENCE:
;       Obj = OBJ_NEW( 'SensorInfo', Debug=Debug )
;
;         or
;
;       Result = Obj->[SensorInfo::]Init( Debug=Debug )  (In a lifecycle method only)
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
; FUNCTION RESULT:
;       Result:      The return value is an integer defining the error
;                    status. The error codes are defined in the error_codes
;                    include file.
;                    If == TRUE the object creation was sucessful
;                       == FALSE an unrecoverable error occurred
;                    
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
;       The Init method is invoked when a SensorInfo object is created,
;
;         IDL> x = OBJ_NEW('SensorInfo')
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 01-Oct-2008
;                       paul.vandelst@noaa.gov
;
;-

FUNCTION SensorInfo::Init, Debug=Debug  ; Input keyword

  ; Set up
  ; ...Parameters
  @sensorinfo_parameters
  ; ...error handler
  @sensorinfo_func_err_handler


  ; Set default values
  self.Sensor_Name      = ' '
  self.Satellite_Name   = ' '
  self.Sensor_Id        = ' '
  self.WMO_Satellite_Id = INVALID_WMO_SATELLITE_ID
  self.WMO_Sensor_Id    = INVALID_WMO_SENSOR_ID
  self.Sensor_Type      = INVALID_SENSOR


  ; Done
  RETURN, TRUE
 
END
