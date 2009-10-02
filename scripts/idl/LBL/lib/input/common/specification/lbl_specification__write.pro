;+
; NAME:
;       LBL_Specification::Write
;
; PURPOSE:
;       The LBL_Specification::Write procedure method writes the
;       LBL_Specification object to a file
;
; CALLING SEQUENCE:
;       Obj->[LBL_Specification::]Write, $
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
;       Written by:     Paul van Delst, 21-Jul-2009
;                       paul.vandelst@noaa.gov
;
;-


PRO LBL_Specification::Write, $
  FileId       , $  ;  Input
  Debug = Debug     ;  Input keyword

  ; Set up
  @lbl_specification_parameters
  @lbl_pro_err_handler
  ; ...Check input
  IF ( N_ELEMENTS(FileId) EQ 0 ) THEN $
      MESSAGE, 'Must specify a valid FileId', $
        NONAME=MsgSwitch, NOPRINT=MsgSwitch
  info = FSTAT(FileId)
  IF (info.OPEN EQ 0 ) THEN $
    MESSAGE, 'FileID does not refer to an open file', $
      NONAME=MsgSwitch, NOPRINT=MsgSwitch

  
  ; Get the data
  self->Get_Property, $
    Debug      = Debug     , $
    lbl_type   = lbl_type  , $
    lbl_fmt    = lbl_fmt   , $
    v1         = v1        , $
    v2         = v2        , $
    sample     = sample    , $
    dvset      = dvset     , $
    alfal0     = alfal0    , $
    avmass     = avmass    , $
    dptmin     = dptmin    , $
    dptfac     = dptfac    , $
    ilnflg     = ilnflg    , $
    dvout      = dvout     , $
    nmol_scale = nmol_scale, $
    hmol_scale = hmol_scale, $
    xmol_scale = xmol_scale
  
stop  
  ; Output record
  CASE lbl_type OF

    ASCII_LBL_TYPE: BEGIN
      PRINTF, FileId, FORMAT=lbl_fmt, $
        v1        , $
        v2        , $
        sample    , $
        dvset     , $
        alfal0    , $
        avmass    , $
        dptmin    , $
        dptfac    , $
        ilnflg    , $
        dvout     , $
        nmol_scale
    END

    LBLRTM_LBL_TYPE: BEGIN
      PRINTF, FileId, FORMAT=lbl_fmt, $
        v1        , $
        v2        , $
        sample    , $
        dvset     , $
        alfal0    , $
        avmass    , $
        dptmin    , $
        dptfac    , $
        ilnflg    , $
        dvout     , $
        nmol_scale
    END

    MONORTM_LBL_TYPE: BEGIN
      PRINTF, FileId, FORMAT=lbl_fmt, $
        v1        , $
        v2        , $
        dvset     , $
        nmol_scale
    END

    ELSE: $
      MESSAGE, 'Invalid LBL type!', $
        NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ENDCASE


  ; Done
  CATCH, /CANCEL

END ; PRO LBL_Specification::Write
