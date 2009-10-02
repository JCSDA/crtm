;+
; NAME:
;       LBL_Input::Add
;
; PURPOSE:
;       The LBL_Input::Add procedure method adds an object references to
;       the LBL_Input container.
;
;       This method overrides the IDL_Container::Add function method.
;
; CALLING SEQUENCE:
;       Obj->[LBL_Input::]Add, $
;         Obj_to_Add, $  ; Input
;         Debug = Debug  ; Input keyword
;
; INPUT ARGUMENTS:
;         Obj_to_Add: Object to add to the LBL_Input container, Obj.
;                     UNITS:      N/A
;                     TYPE:       Object
;                     DIMENSION:  Scalar
;                     ATTRIBUTES: INTENT(IN), OPTIONAL
;
; KEYWORDS:
;       Any keywords to the IDL_Container::Add method can be used.
;       In addition, the following keywords are available:
;
;       Debug:        Set this keyword for debugging.
;                     If NOT SET => Error handler is enabled. (DEFAULT)
;                        SET     => Error handler is disabled; Routine
;                                   traceback output is enabled.
;                     UNITS:      N/A
;                     TYPE:       INTEGER
;                     DIMENSION:  Scalar
;                     ATTRIBUTES: INTENT(IN), OPTIONAL
;
; INCLUDE FILES:
;       lbl_parameters: Include file containing LBL specific
;                       parameter value definitions.
;
;       lbl_pro_err_handler: Error handler code for LBL procedures.
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 21-Jul-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO LBL_Input::Add, $
  obj, $
  Debug      = Debug, $  ; Input keyword
  _REF_EXTRA = Extra     ; Keywords passed onto IDL_Container::Add
 
  ; Set up
  @lbl_parameters
  @lbl_pro_err_handler


  ; Get the LBL type
  self->Get_Property, lbl_type=lbl_type, Debug=Debug
  
  
  ; Ensure the object to be added is of the same type
  IF ( OBJ_ISA(obj,'LBL_Base') ) THEN obj->Set_Property, lbl_type=lbl_type, Debug=Debug


  ; Add generic object object to the container
  self->IDL_Container::Add, obj, _EXTRA = Extra


  ; Done
  CATCH, /CANCEL

END ; PRO LBL_Input::Add
