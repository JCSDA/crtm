;+
; NAME:
;       SensorInfo_List::Init
;
; PURPOSE:
;       The SensorInfo_List::Init function method initialises a SensorInfo_List
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
;       Obj = OBJ_NEW( 'SensorInfo_List', [Filename,] [PROPERTY=value,] Debug=Debug )
;
;         or, in a lifecycle method only,
;
;       Result = Obj->[SensorInfo_List::]Init( [Filename, $]
;                                              [PROPERTY=value, $]
;                                              Debug=Debug )
;
; ARGUMENTS:
;       Filename:    Specify this argument to set the filename
;                    associated with the SensorInfo_List object. If not
;                    supplied, the default value is "SensorInfo"
;                    UNITS:      N/A
;                    TYPE:       CHARACTER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
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
;       PROPERTY:    Any of the group of keywords accepted by the
;                    SensorInfo_List::Set_Property procedure method.
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
;       sensorinfo_parameters: Include file containing SensorInfo specific
;                              parameter value definitions.
;
;       error_codes:    Include file containing error code definitions.
;
; EXAMPLE:
;       The Init method is invoked when a SensorInfo_List object is created,
;
;         IDL> list = OBJ_NEW('SensorInfo_List')
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 02-Oct-2008
;                       paul.vandelst@noaa.gov
;
;-

FUNCTION SensorInfo_List::Init, $
  Filename, $
  Debug = Debug, $
  _EXTRA = Properties

  ; Set up
  ; ...Parameters
  @sensorinfo_parameters
  ; ...Set up error handler
  @error_codes
  IF ( KEYWORD_SET(Debug) ) THEN BEGIN
    MESSAGE, '--> Entered.', /INFORMATIONAL
  ENDIF ELSE BEGIN
    CATCH, Error_Status
    IF ( Error_Status NE 0 ) THEN BEGIN
      CATCH, /CANCEL
      MESSAGE, !ERROR_STATE.MSG, /CONTINUE
      RETURN, FAILURE
    ENDIF
  ENDELSE
  ; ..Check input
  IF ( Valid_String(Filename) ) THEN $
    _Filename = Filename $
  ELSE $
    _Filename = 'SensorInfo'

  ; Set default values
  self.Filename  = _Filename
  self.n_Sensors = 0L


  ; Set supplied properties.
  self->Set_Property, _EXTRA = Properties
  
  
  ; Done
  CATCH, /CANCEL
  RETURN, TRUE
 
END ; FUNCTION SensorInfo_List::Init
