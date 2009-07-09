;+
; NAME:
;       OSRF_File::Add
;
; PURPOSE:
;       The OSRF_File::Add procedure method adds an object references to
;       the OSRF_File container.
;
;       This method overrides the IDL_Container::Add function method.
;
; CALLING SEQUENCE:
;       Obj->[OSRF_File::]Add, $
;         Obj_to_Add, $  ; Input
;         Debug = Debug  ; Input keyword
;
; INPUT ARGUMENTS:
;         Obj_to_Add: Object to add to the AtmProfile_File container, Obj.
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
;       osrf_file parameters: Include file containing OSRF_File specific
;                             parameter value definitions.
;
;       osrf_func_err_handler: Error handler code for OSRF procedures.
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 01-Jul-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO OSRF_File::Add, $
  obj, $
  Debug      = Debug, $  ; Input keyword
  _REF_EXTRA = Extra     ; Keywords passed onto IDL_Container::Add
 
  ; Set up
  ; ...OSRF parameters
  @osrf_file_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler


  ; Add object to the container
  self->IDL_Container::Add, obj, _EXTRA = Extra
  
  
  ; If the object added is an OSRF object, increment the channel counter
  IF ( OBJ_ISA(obj, 'OSRF') ) THEN self.n_Channels++


  ; Done
  CATCH, /CANCEL

END ; PRO OSRF_File::Add
