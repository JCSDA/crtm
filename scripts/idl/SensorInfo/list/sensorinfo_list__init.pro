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
;       Obj = OBJ_NEW( 'SensorInfo_List', Debug=Debug )
;
;         or, in a lifecycle method only,
;
;       Result = Obj->[SensorInfo_List::]Init( Debug=Debug )
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

FUNCTION SensorInfo_List::Init, Debug=Debug  ; Input keyword

  ; Set up error handler
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

  ; Set default values
  self.n_Nodes = 0L
  self.First   = PTR_NEW({SensorInfo_Node})

  CATCH, /CANCEL
  RETURN, SUCCESS
 
END ; FUNCTION SensorInfo_List::Init
