;+
; NAME:
;       LBL_Solar::Write
;
; PURPOSE:
;       The LBL_Solar::Write procedure method writes the
;       LBL_Solar object to a file
;
; CALLING SEQUENCE:
;       Obj->[LBL_Solar::]Write, $
;         FileId       , $  ;  Input
;         Debug = Debug     ;  Input keyword
;
; INPUT:
;       FileId:      The file id to which to write. Must be an id for
;                    a valid, open file.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN)
;
; KEYWORDS:
;       Debug:       Set this keyword for debugging.
;                    If NOT SET => Error handler is enabled. (DEFAULT)
;                       SET     => Error handler is disabled; Routine
;                                  traceback output is enabled.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 16-Jul-2009
;                       paul.vandelst@noaa.gov
;
;-


PRO LBL_Solar::Write, $
  FileId       , $  ;  Input
  Debug = Debug     ;  Input keyword

  ; Set up
  @lbl_solar_parameters
  @lbl_pro_err_handler
  ; ...Check input
  IF ( N_ELEMENTS(FileId) EQ 0 ) THEN $
      MESSAGE, 'Must specify a valid FileId', $
        NONAME=MsgSwitch, NOPRINT=MsgSwitch
  info = FSTAT(FileId)
  IF (info.OPEN EQ 0 ) THEN $
    MESSAGE, 'FileID does not refer to an open file', $
      NONAME=MsgSwitch, NOPRINT=MsgSwitch

 
  ; Get the data and format
  self->Get_Property, $
    Debug    = Debug   , $
    lbl_type = lbl_type, $
    lbl_fmt  = lbl_fmt , $
    inflag   = inflag  , $
    iotflag  = iotflag , $
    juldat   = juldat    


  ; Output solar data record
  PRINTF, FileId, FORMAT=lbl_fmt, $
    inflag , $
    iotflag, $
    juldat 


  ; Done
  CATCH, /CANCEL

END ; PRO LBL_Solar::Write
