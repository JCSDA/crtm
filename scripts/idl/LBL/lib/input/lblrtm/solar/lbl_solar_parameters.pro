;+

  @lbl_parameters
  
  LBL_SOLAR_FMT = [ $
    ; ASCII format
    '(2x,"LBL_SOLAR Record:",/,'+$
     '4x,"INFLAG  : ",i5,/,'+$
     '4x,"IOTFLAG : ",i5,/,'+$
     '4x,"JULDAT  : ",i5 )', $
    ; LBLRTM format
    '(i5,i5,2x,i3)', $
    ; MonoRTM format
    '("Not a MonoRTM record")' ]

;-
