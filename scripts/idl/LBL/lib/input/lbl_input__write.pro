;+
; NAME:
;       LBL_Input::Write
;
; PURPOSE:
;       The LBL_Input::Write procedure method writes all the contained
;       LBL objects to an LBL input file
;
; CALLING SEQUENCE:
;       Obj->[LBL_Input::]Write, $
;         Debug = Debug         ;  Input keyword
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
; INCLUDE FILES:
;       lbl_parameters: Include file containing lbl specific
;                       parameter value definitions.
;
;       lbl_pro_err_handler: Error handler code for lbl procedures.
;
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 17-Jul-2009
;                       paul.vandelst@noaa.gov
;
;-


PRO LBL_Input::Write, $
  Debug = Debug     ;  Input keyword

  ; Set up
  @lbl_parameters
  @lbl_pro_err_handler


  ; Return if no objects in container
  IF ( self->Count() EQ 0 ) THEN RETURN


  ; Open the file for writing
  GET_LUN, FileId
  OPENW, FileId, self.filename
  

  ; Start writing the input file
  ; ----------------------------
  
  ; The UserId
  obj_name = 'LBL_UserId'
  lbl_userid = self->Get(/ALL, ISA=obj_name)
  lbl_userid[0]->Write, FileId, Debug = Debug
  
  
  ; The Control
  obj_name = 'LBL_Control'
  lbl_control = self->Get(/ALL, ISA=obj_name)
  lbl_control[0]->Write, FileId, Debug = Debug
  ; ...Get info for checking
  lbl_control[0]->Get_Property, Debug     = Debug, $
                                lbl_type  = lbl_type , $
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
                                speed     = speed       
                                

  ; Continua factors
  IF ( continuum EQ 6 ) THEN BEGIN
    obj_name = 'LBL_Continuum'
    lbl_continuum = self->Get(/ALL, ISA=obj_name)
    lbl_continuum[0]->Write, FileId, Debug = Debug
  ENDIF
  
  
  ; Solar record
  IF ( lbl_type EQ LBLRTM_LBL_TYPE AND emit EQ 2 ) THEN BEGIN
    obj_name = 'LBL_Solar'
    lbl_solar = self->Get(/ALL, ISA=obj_name)
    lbl_solar[0]->Write, FileId, Debug = Debug
  ENDIF
  
  
  ; Specification record
  obj_name = 'LBL_Specification'
  lbl_spec = self->Get(/ALL, ISA=obj_name)
  lbl_spec[0]->Write, FileId, Debug = Debug
  
  
  ; Done
  FREE_LUN, FileId
  CATCH, /CANCEL

END ; PRO LBL_Input::Write
