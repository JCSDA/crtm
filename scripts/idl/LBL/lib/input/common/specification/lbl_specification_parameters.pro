;+

  @lbl_parameters
  
  LBL_SPECIFICATION_FMT = [ $
    ; Formats for generic ASCII output
    [ $
      ; ...Main Record
      '(2x,"LBL_SPECIFICATION Record:",/,'+$
       '4x,"V1         : ",e13.6,/,'+$
       '4x,"V2         : ",e13.6,/,'+$
       '4x,"SAMPLE     : ",e13.6,/,'+$
       '4x,"DVSET      : ",e13.6,/,'+$
       '4x,"ALFAL0     : ",e13.6,/,'+$
       '4x,"AVMASS     : ",e13.6,/,'+$
       '4x,"DPTMIN     : ",e13.6,/,'+$
       '4x,"DPTFAC     : ",e13.6,/,'+$
       '4x,"ILNFLG     : ",i5,/,'+$
       '4x,"DVOUT      : ",e13.6,/,'+$
       '4x,"NMOL_SCALE : ",i5 )', $
      ; ...Profile scale record
      '(2x,"LBL_PROFILESCALE Record:",/,'+$
       '4x,"HMOL_SCALE : ",'+STRTRIM(LBL_N_MOLECULES_MAX,2)+'a1,/,'+$
       '4x,"XMOL_SCALE : ",/,8(e15.7,:) )' ], $

    ; Formats for LBLRTM output
    [ $
      ; ...Main Record
      '(2(f10.3),6(e10.3),4x,i1,5x,e10.3,3x,i2)', $
      ; ...Profile scale record
      '( '+STRTRIM(LBL_N_MOLECULES_MAX,2)+'a1,/,8(e15.7,:) )' ], $

    ; Formats for MonoRTM output
    [ $
      ; ...Main Record
      '(2(e10.3),10x,e10.3,63x,i2)', $
      ; ...Profile scale record
      '( '+STRTRIM(LBL_N_MOLECULES_MAX,2)+'a1,/,7(e15.7,:),/,8(e15.7,:) )' ] ]
    

  ; Input defaults
  LBL_SPECIFICATION_BANDWIDTH_MAX  = 2020.0d0
  LBL_SPECIFICATION_SAMPLE_MIN     = ONE
  LBL_SPECIFICATION_SAMPLE_MAX     = 4.0d0
  LBL_SPECIFICATION_SAMPLE_DEFAULT = LBL_SPECIFICATION_SAMPLE_MAX
  LBL_SPECIFICATION_ALFAL0_DEFAULT = 0.04d0
  LBL_SPECIFICATION_AVMASS_DEFAULT = 36.0d0
  LBL_SPECIFICATION_DPTMIN_DEFAULT = 0.0002d0  ; For -ve specified values
  LBL_SPECIFICATION_DPTFAC_DEFAULT = 0.001d0   ; For -ve specified values
  
;-
