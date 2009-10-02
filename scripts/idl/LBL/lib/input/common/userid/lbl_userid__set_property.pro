;+
; NAME:
;       LBL_UserId::Set_Property
;
; PURPOSE:
;       The LBL_UserId::Set_Property procedure method sets the value
;       of a property or group of properties for this object.
;
; CALLING SEQUENCE:
;       Obj->[LBL_UserId::]Set_Property, $
;         Debug          = Debug         , $  ; Input keyword
;         Identification = Identification, $  ; Input keyword
;         lbl_type       = lbl_type           ; Input keyword
;
; KEYWORDS:
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
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       lbl_type:              Integer flag used to define the type of LBL input
;                              file. Valid values are defined in the lbl_parameters
;                              include file.
;                              If not specified, the default value is 0 (ASCII).
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 16-Jul-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO LBL_UserId::Set_Property, $
  Debug          = Debug         , $  ; Input keyword
  Identification = Identification, $  ; Input keyword
  lbl_type       = lbl_type      , $  ; Input keyword (used for LBL_Base)
  _EXTRA         = ignore
  
  ; Set up
  @lbl_userid_parameters
  @lbl_pro_err_handler

 
  ; Set subclass data
  IF ( Valid_String(Identification) ) THEN self.Identification = STRTRIM(Identification,2)

  
  ; Set superclass data
  IF ( N_ELEMENTS(lbl_type) GT 0 ) THEN type = LONG(lbl_type[0]) ELSE type = 0L
  name = LBL_TYPE_NAME[type]
  fmt  = LBL_USERID_FMT[type]
  self->LBL_Base::Set_Property, $
    Debug = Debug, $
    lbl_type = type, $
    lbl_name = name, $
    lbl_fmt  = fmt
  
  
  ; Done
  CATCH, /CANCEL
 
END ; PRO LBL_UserId::Set_Property
