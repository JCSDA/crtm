;+
; NAME:
;       Cloud::Cleanup
;
; PURPOSE:
;       The Cloud::Cleanup procedure method performs cleanup
;       on a Cloud object when it is destroyed.
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
;       Obj->[Cloud::]Cleanup, Debug=Debug  (In a lifecycle method only)
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
;-

PRO Cloud::Cleanup, Debug=Debug  ; Input keyword

  ; Set up
  COMPILE_OPT HIDDEN
  @cloud_pro_err_handler
 

  ; Cleanup
  self->Destroy, Debug = Debug
 
END
