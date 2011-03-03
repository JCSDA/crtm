;+
; NAME:
;       BitFlag::Cleanup
;
; PURPOSE:
;       The BitFlag::Cleanup procedure method performs cleanup
;       on a BitFlag object when it is destroyed.
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
;       Obj->[BitFlag::]Cleanup, Debug=Debug  (In a lifecycle method only)
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
;       bitflag_parameters: Include file containing BitFlag specific
;                            parameter value definitions.
;
;       bitflag_pro_err_handler: Error handler code for BitFlag procedures.
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 28-Sep-2010
;                       paul.vandelst@noaa.gov
;
;-

PRO BitFlag::Cleanup, $
  Debug=Debug  ; Input keyword
 
  ; Set up
  COMPILE_OPT HIDDEN
  ; ...BitFlag parameters
  @bitflag_parameters
  ; ...Set up error handler
  @bitflag_pro_err_handler


  ; Hook for future changes


END ; PRO BitFlag::Cleanup
