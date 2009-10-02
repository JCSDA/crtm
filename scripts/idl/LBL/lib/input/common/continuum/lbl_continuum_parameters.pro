;+

  @lbl_parameters
  
  LBL_CONTINUUM_FMT = [ $
    ; ASCII format
    '(2x,"LBL_CONTINUUM Record:",/,'+$
     '4x,"H2O_SELF   : ",f8.5,/,'+$
     '4x,"H2O_FOREIGN: ",f8.5,/,'+$
     '4x,"CO2        : ",f8.5,/,'+$
     '4x,"O3         : ",f8.5,/,'+$
     '4x,"O2         : ",f8.5,/,'+$
     '4x,"N2         : ",f8.5,/,'+$
     '4x,"RAYLEIGH   : ",f8.5 )', $
    ; LBLRTM format
    '( 7(2x,f8.5) )', $
    ; MonoRTM format
    '( 7(2x,f8.5) )' ]

;-
