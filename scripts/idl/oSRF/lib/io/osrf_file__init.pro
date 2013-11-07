;+
; NAME:
;       OSRF_File::Init
;
; PURPOSE:
;       The OSRF_File::Init function method initialises an OSRF_File object.
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
;       Obj = OBJ_NEW( 'OSRF_File', [Filename,] [PROPERTY=value,] Debug=Debug )
;
;         or, in a lifecycle method,
;
;       Result = Obj->[OSRF::]Init( Filename, $ 
;                                   [PROPERTY=value, $]
;                                   Debug=Debug )
;
; OPTIONAL INPUT ARGUMENTS:
;       Filename:    Specify this argument to set the filename
;                    associated with the OSRF_File object. If not
;                    supplied, the default value is "osrf_file.srf.nc"
;                    UNITS:      N/A
;                    TYPE:       CHARACTER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
;
; INPUT KEYWORDS:
;       Debug:       Set this keyword for debugging.
;                    If NOT SET => Error handler is enabled. (DEFAULT)
;                       SET     => Error handler is disabled; Routine
;                                  traceback output is enabled.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       PROPERTY:    Any of the group of keywords accepted by the
;                    OSRF_File::Set_Property procedure method.
;
; FUNCTION RESULT:
;       Result:      The return value is an integer defining the object
;                    initialisation status. The return codes are defined
;                    in the error_codes include file.
;                    If == TRUE the object creation was sucessful
;                       == FALSE an unrecoverable error occurred
;                    
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;
; INCLUDE FILES:
;       osrf_file parameters: Include file containing OSRF_File specific
;                             parameter value definitions.
;
;       error_codes: Include file containing error code definitions.
;
; EXAMPLE:
;       The Init method is invoked when an OSRF object is created,
;
;         IDL> x = OBJ_NEW('OSRF_File', 'test.nc')
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 22-Jun-2009
;                       paul.vandelst@noaa.gov
;
;-

FUNCTION OSRF_File::Init, $
  Filename, $
  Debug  = Debug, $    ; Input keyword
  _EXTRA = Properties  ; Keywords passed onto OSRF_File::Set_Property

  ; Set up
  COMPILE_OPT HIDDEN
  ; ...netCDF parameters
  @osrf_file_parameters
  ; ...Set up error handler
  @osrf_func_err_handler
  ; ..Check input
  IF ( Valid_String(Filename) ) THEN $
    _Filename = Filename $
  ELSE $
    _Filename = 'osrf_file.srf.nc'
 

  ; Set default values
  self.filename = _Filename
  self.Release  = OSRF_RELEASE
  self.Version  = OSRF_VERSION
  
  self.Sensor_Id        = ''
  self.WMO_Satellite_Id = INVALID_WMO_SATELLITE_ID
  self.WMO_Sensor_Id    = INVALID_WMO_SENSOR_ID
  self.Sensor_Type      = INVALID_SENSOR

  self.Title            = ''
  self.History          = ''
  self.Comment          = ''


  ; Set supplied properties.
  self->Set_Property, _EXTRA = Properties
  
  
  
  CATCH, /CANCEL
  RETURN, TRUE
 
END ; FUNCTION OSRF_File::Init
