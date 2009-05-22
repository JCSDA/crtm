;+
; NAME:
;       OSRF::Cleanup
;
; PURPOSE:
;       The OSRF::Cleanup procedure method performs cleanup
;       on a OSRF object when it is destroyed.
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
;       Obj->[OSRF::]Cleanup, Debug=Debug  (In a lifecycle method only)
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
;       srf_parameters: Include file containing SensorInfo specific
;                       parameter value definitions.
;
;       osrf_func_err_handler: Error handler code for OSRF functions.
;
; EXAMPLE:
;       After creating and allocating a OSRF object, e.g.
;
;         IDL> x = OBJ_NEW('OSRF')
;         IDL> Result = x->Allocate(10)
;
;       the Cleanup method is invoked when the object is destroyed,
;
;         IDL> OBJ_DESTROY, x
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 20-Apr-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO OSRF::Cleanup, Debug=Debug  ; Input keyword
 
  ; Set up
  ; ...Generic SRF parameters
  @srf_parameters
  
  ; ...Set up error handler
  @osrf_func_err_handler


  ; Deallocate pointers, and clear scalars
  Result = self->Destroy(Debug=Debug)
  IF ( Result NE SUCCESS ) THEN $
    MESSAGE, 'Error destroying OSRF structure', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch

  ; Done
  Done:
  CATCH, /CANCEL

END ; PRO OSRF::Cleanup
