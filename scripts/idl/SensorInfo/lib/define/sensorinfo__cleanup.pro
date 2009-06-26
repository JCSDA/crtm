;+
; NAME:
;       SensorInfo::Cleanup
;
; PURPOSE:
;       The SensorInfo::Cleanup procedure method performs cleanup
;       on a SensorInfo object when it is destroyed.
;
;       NOTE: Cleanup methods are special *lifecycle methods* and, as
;             such, cannot be called outside the context of object
;             creation and destruction. This means that in most cases
;             you cannot call the Cleanup method directly. There is one
;             exception to this rule: if you write your own subclass of
;             this class, you can call the Cleanup method from within the
;             Init or Cleanup method of the subclass.
;
; CALLING SEQUENCE:
;       OBJ_DESTROY, Obj
;
;         or
;
;       Obj->[SensorInfo::]Cleanup, Debug=Debug  (In a lifecycle method only)
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
; INCLUDE FILES:
;       sensorinfo_parameters: Include file containing SensorInfo specific
;                              parameter value definitions.
;
;       error_codes:           Include file containing error code definitions.
;
; EXAMPLE:
;       After creating and allocating a SensorInfo object, e.g.
;
;         IDL> x = OBJ_NEW('SensorInfo')
;         IDL> Result = x->Allocate(10)
;
;       the Cleanup method is invoked when the object is destroyed,
;
;         IDL> OBJ_DESTROY, x
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 01-Oct-2008
;                       paul.vandelst@noaa.gov
;
;-

PRO SensorInfo::Cleanup, Debug=Debug  ; Input keyword
 
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
      RETURN
    ENDIF
    MsgSwitch = 1
  ENDELSE


  ; Deallocate pointers, and clear scalars
  ; --------------------------------------
  Result = self->Destroy(Debug=Debug)
  IF ( Result NE SUCCESS ) THEN $
    MESSAGE, 'Error destroying SensorInfo structure', NONAME=MsgSwitch, NOPRINT=MsgSwitch

  ; Done
  Done:
  CATCH, /CANCEL

END ; PRO SensorInfo::Cleanup
