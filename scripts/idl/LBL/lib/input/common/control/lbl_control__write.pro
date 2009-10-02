;+
; NAME:
;       LBL_Control::Write
;
; PURPOSE:
;       The LBL_Control::Write procedure method writes the
;       LBL_Control object to a file
;
; CALLING SEQUENCE:
;       Obj->[LBL_Control::]Write, $
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


PRO LBL_Control::Write, $
  FileId       , $  ;  Input
  Debug = Debug     ;  Input keyword

  ; Set up
  @lbl_control_parameters
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
    Debug     = Debug    , $
    hirac     = hirac    , $
    lblf4     = lblf4    , $
    continuum = continuum, $
    aerosol   = aerosol  , $
    emit      = emit     , $
    scan      = scan     , $
    filter    = filter   , $
    plotlbl   = plotlbl  , $
    test      = test     , $
    atm       = atm      , $
    merge     = merge    , $
    laser     = laser    , $
    od        = od       , $
    xsection  = xsection , $
    mpts      = mpts     , $
    npts      = npts     , $
    speed     = speed    , $
    lbl_type  = lbl_type , $
    lbl_fmt   = lbl_fmt   
  
  
  ; Output control record
  CASE lbl_type OF

    ASCII_LBL_TYPE: BEGIN
      PRINTF, FileId, FORMAT=lbl_fmt, $
        hirac    , $
        lblf4    , $
        continuum, $
        aerosol  , $
        emit     , $
        scan     , $
        filter   , $
        plotlbl  , $
        test     , $
        atm      , $
        merge    , $
        laser    , $
        od       , $
        xsection , $
        mpts     , $
        npts     , $
        speed    
    END

    LBLRTM_LBL_TYPE: BEGIN
      PRINTF, FileId, FORMAT=lbl_fmt, $
        hirac    , $
        lblf4    , $
        continuum, $
        aerosol  , $
        emit     , $
        scan     , $
        filter   , $
        plotlbl  , $
        test     , $
        atm      , $
        merge    , $
        laser    , $
        od       , $
        xsection , $
        mpts     , $
        npts     
    END

    MONORTM_LBL_TYPE: BEGIN
      PRINTF, FileId, FORMAT=lbl_fmt, $
        hirac    , $
        continuum, $
        atm      , $
        xsection , $
        speed    
    END

    ELSE: $
      MESSAGE, 'Invalid LBL type!', $
        NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ENDCASE


  ; Done
  CATCH, /CANCEL

END ; PRO LBL_Control::Write
