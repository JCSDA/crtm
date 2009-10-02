;+
; NAME:
;       LBL_UserId::Get_Property
;
; PURPOSE:
;       The LBL_UserId::Get_Property procedure method gets the value
;       of a property or group of properties for this object.
;
; CALLING SEQUENCE:
;       Obj->[LBL_UserId::]Get_Property, $
;         Debug          = Debug         , $  ; Input keyword
;         Identification = Identification     ; Output keyword
;
; KEYWORDS:
;       Along with any of the group of keywords accepted by the
;       LBL_Base::Get_Property procedure method, the following
;       keywords are also accepted:
;
;       Debug:                 Set this keyword for debugging.
;                              If NOT SET => Error handler is enabled. (DEFAULT)
;                                 SET     => Error handler is disabled; Routine
;                                            traceback output is enabled.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Identification:        The user identification string.
;                              Maximum of 79 useful characters.
;                              UNITS:      N/A
;                              TYPE:       CHARACTER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(OUT), OPTIONAL
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 20-Jul-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO LBL_UserId::Get_Property, $
  Debug          = Debug         , $  ; Input keyword
  Identification = Identification, $  ; Output keyword
  _REF_EXTRA     = Extra              ; Keywords passed onto LBL_Base::Get_Property
  
  ; Set up
  @lbl_parameters
  @lbl_pro_err_handler
 

  ; Get subclass data
  IF ( ARG_PRESENT(Identification) ) THEN Identification = self.Identification
  
  
  ; Get superclass data
  self->LBL_Base::Get_Property, _EXTRA = Extra, Debug = Debug
  
  
  ; Done
  CATCH, /CANCEL
 
END ; PRO LBL_UserId::Get_Property
