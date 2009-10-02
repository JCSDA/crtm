;+
; Parameters for LBL routines

  ; Literal constants
  ZERO = 0.0d0
  ONE  = 1.0d0

  
  ; LBL type flags
  ASCII_LBL_TYPE   = 0L
  LBLRTM_LBL_TYPE  = 1L
  MONORTM_LBL_TYPE = 2L
  LBL_TYPE_NAME = [ 'ASCII'  , $
                    'LBLRTM' , $
                    'MonoRTM'  ]
  N_LBL_TYPES = N_ELEMENTS(LBL_TYPE_NAME)


  ; Generic limits
  LBL_N_MOLECULES_MAX = 39L
  
;-
