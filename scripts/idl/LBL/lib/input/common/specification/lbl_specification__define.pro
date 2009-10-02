;+
; LBL_Specification record definition procedure
;
; In LBLRTM  :: Record 1.3
;    MonoRTM :: Record 1.3

PRO LBL_Specification__Define

;-

  @lbl_specification_parameters
  
  void = { LBL_Specification, $
           v1         : 0.0d0, $  ; beginning wavenumber value for the calculation
           v2         : 0.0d0, $  ; ending wavenumber value for the calculation
           sample     : 0.0d0, $  ; number of sample points per mean halfwidth
           dvset      : 0.0d0, $  ; selected DV for the final monochromatic calculation
           alfal0     : 0.0d0, $  ; average collision broadened halfwidth
           avmass     : 0.0d0, $  ; average molecular mass (amu) for Doppler halfwidth
           dptmin     : 0.0d0, $  ; minimum molecular optical depth below which lines will be rejected
           dptfac     : 0.0d0, $  ; factor multiplying molecular continuum optical depth
           ilnflg     : 0L   , $  ; flag for binary record of line rejection information
           dvout      : 0.0d0, $  ; selected DV grid for the optical depth "monochromatic" output spacing
           nmol_scale : 0L   , $  ; Enables the scaling of the atmospheric profile for selected species
           hmol_scale : STRARR(LBL_N_MOLECULES_MAX), $  ; Profile scaling definitions
           xmol_scale : DBLARR(LBL_N_MOLECULES_MAX), $  ; Profile scaling factors
           INHERITS LBL_Base  }  ; Shared base object

END ; PRO LBL_Specification__Define
