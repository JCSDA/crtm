;+
; LBL_Continuum record definition procedure
;
; In LBLRTM  :: Record 1.2a
;    MonoRTM :: Record 1.2a

PRO LBL_Continuum__Define

;-

  void = { LBL_Continuum, $
           h2o_self    : 0.0d0, $  ; H2O self broadened continuum absorption multiplicative factor
           h2o_foreign : 0.0d0, $  ; H2O foreign broadened continuum absorption multiplicative factor
           co2         : 0.0d0, $  ; CO2 continuum absorption multiplicative factor
           o3          : 0.0d0, $  ; O3 continuum absorption multiplicative factor
           o2          : 0.0d0, $  ; O2 continuum absorption multiplicative factor
           n2          : 0.0d0, $  ; N2 continuum absorption multiplicative factor
           rayleigh    : 0.0d0, $  ; Rayleigh extinction multiplicative factor
           INHERITS LBL_Base    }  ; Shared base object

END ; PRO LBL_Continuum__Define
