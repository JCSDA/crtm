;+
; NAME:
;       OSRF::Init
;
; PURPOSE:
;       The OSRF::Init function method initialises an OSRF object.
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
;       Obj = OBJ_NEW( 'OSRF', Debug=Debug )
;
;         or
;
;       Result = Obj->[OSRF::]Init( Debug=Debug )  (In a lifecycle method only)
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
;
; FUNCTION RESULT:
;       Result:      The return value is an integer defining the error
;                    status. The error codes are defined in the error_codes
;                    include file.
;                    If == SUCCESS the object creation was sucessful
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
;       The Init method is invoked when an OSRF object is created,
;
;         IDL> x = OBJ_NEW('OSRF')
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 20-Apr-2009
;                       paul.vandelst@noaa.gov
;
;-

FUNCTION OSRF::Init, Debug=Debug  ; Input keyword

  ; Set up
  ; ...Generic SRF parameters
  @srf_parameters
  
  ; ...Set up error handler
  @osrf_func_err_handler
 

  ; Set default values
  self.Release          = SRF_RELEASE
  self.Version          = SRF_VERSION
  self.Sensor_Id        = ' '
  self.WMO_Satellite_Id = INVALID_WMO_SATELLITE_ID
  self.WMO_Sensor_Id    = INVALID_WMO_SENSOR_ID
  self.Sensor_Type      = INVALID_SENSOR
  self.Channel          = INVALID
  self.Integral         = ZERO
  self.Flags            = 0L  ; Clear all flags

  CATCH, /CANCEL
  RETURN, SUCCESS
 
END ; FUNCTION OSRF::Init
