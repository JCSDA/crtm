;+
; NAME:
;       AtmProfile_File::Cleanup
;
; PURPOSE:
;       The AtmProfile_File::Cleanup procedure method performs cleanup
;       on a AtmProfile_File object when it is destroyed.
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
;       Obj->[AtmProfile_File::]Cleanup, Debug=Debug  (In a lifecycle method only)
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
; INCLUDE FILES:
;       atmprofile_file_parameters: Include file containing AtmProfile_File specific
;                                   parameter value definitions.
;
;       atmprofile_pro_err_handler: Error handler code for AtmProfile procedures.
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 06-Jul-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO AtmProfile_File::Cleanup, $
  Debug=Debug
 
  ; Set up
  COMPILE_OPT HIDDEN
  ; ...AtmProfile parameters
  @atmprofile_file_parameters
  ; ...Set up error handler
  @atmprofile_pro_err_handler


  ; Empty container
  self->IDL_Container::Cleanup


  ; Done
  CATCH, /CANCEL

END ; PRO AtmProfile_File::Cleanup
