;+
; LBL_Control record definition procedure
;
; In LBLRTM  :: Record 1.2
;    MonoRTM :: Record 1.2

PRO LBL_Control__Define

;-

  void = { LBL_Control, $
           hirac     : 0L, $
           lblf4     : 0L, $
           continuum : 0L, $
           aerosol   : 0L, $
           emit      : 0L, $
           scan      : 0L, $
           filter    : 0L, $
           plotlbl   : 0L, $
           test      : 0L, $
           atm       : 0L, $
           merge     : 0L, $
           laser     : 0L, $
           od        : 0L, $
           xsection  : 0L, $
           mpts      : 0L, $
           npts      : 0L, $
           speed     : 0L, $
           INHERITS LBL_Base }  ; Shared base object

END ; PRO LBL_Control__Define
