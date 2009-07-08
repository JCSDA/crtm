;+
; NAME:
;       AtmProfile::Cleanup
;
; PURPOSE:
;       The AtmProfile::Cleanup procedure method performs cleanup
;       on a AtmProfile object when it is destroyed.
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
;       Obj->[AtmProfile::]Cleanup, Debug=Debug  (In a lifecycle method only)
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
;       atmprofile_parameters: Include file containing AtmProfile specific
;                              parameter value definitions.
;
;       atmprofile_pro_err_handler: Error handler code for AtmProfile procedures.
;
; EXAMPLE:
;       After creating and allocating a AtmProfile object, e.g.
;
;         IDL> x = OBJ_NEW('AtmProfile')
;         IDL> x->Allocate, 10
;
;       the Cleanup method is invoked when the object is destroyed,
;
;         IDL> OBJ_DESTROY, x
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 01-Jul-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO AtmProfile::Cleanup, Debug=Debug  ; Input keyword
 
  ; Set up
  COMPILE_OPT HIDDEN
  ; ...AtmProfile parameters
  @atmprofile_parameters
  ; ...Set up error handler
  @atmprofile_pro_err_handler


  ; Deallocate pointers, and clear scalars
  self->Destroy, Debug=Debug


  ; Done
  Done:
  CATCH, /CANCEL

END ; PRO AtmProfile::Cleanup
