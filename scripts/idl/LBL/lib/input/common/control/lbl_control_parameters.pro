;+

  @lbl_parameters
  
  LBL_CONTROL_FMT = [ $
    ; ASCII format
    '(2x,"LBL_CONTROL Record:",/,'+$
     '4x,"HIRAC     : ",i5,/,'+$
     '4x,"LBLF4     : ",i5,/,'+$
     '4x,"CONTINUUM : ",i5,/,'+$
     '4x,"AEROSOL   : ",i5,/,'+$
     '4x,"EMIT      : ",i5,/,'+$
     '4x,"SCAN      : ",i5,/,'+$
     '4x,"FILTER    : ",i5,/,'+$
     '4x,"PLOTLBL   : ",i5,/,'+$
     '4x,"TEST      : ",i5,/,'+$
     '4x,"ATM       : ",i5,/,'+$
     '4x,"MERGE     : ",i5,/,'+$
     '4x,"LASER     : ",i5,/,'+$
     '4x,"OD        : ",i5,/,'+$
     '4x,"XSECTION  : ",i5,/,'+$
     '4x,"MPTS      : ",i5,/,'+$
     '4x,"NPTS      : ",i5,/,'+$
     '4x,"SPEED     : ",i5 )', $
    ; LBLRTM format
    '(" HI=",i1," F4=",i1," CN=",i1," AE=",i1," EM=",i1," SC=",i1," FI=",i1,"' + $
      ' PL=",i1," TS=",i1," AM=",i1," M=",i2.2," LA=",i1," OD=",i1," XS=",i1,' + $
      ' 1x, i4, 1x, i4 )', $
    ; MonoRTM format
    '(" HI=",i1,5x," CN=",i1,30x," AM=",i1,15x," XS=",i1,12x," SP=",i4)' ]

;-
