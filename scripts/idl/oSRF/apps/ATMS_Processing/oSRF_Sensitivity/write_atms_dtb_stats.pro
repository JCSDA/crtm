PRO Read_Tb_data, AtmProfile_Id, $  ; Input
                  Path         , $  ; Input
                  Tb                ; Output
  ; Get info
  info = SIZE(Tb,/STRUCT)
  ; Read the profile data results
  FOR m = 0, info.DIMENSIONS[0]-1 DO BEGIN
    filename = Path+"/"+AtmProfile_Id+'.Profile'+STRING(m+1,FORMAT='(i4.4)')+'.Tb.dat'
    read_atms_tb, filename, x
    Tb[m,*] = x
  ENDFOR
END


;+
; Procedure to write the ATMS oSRF dTb residual data statistics.
;
PRO write_atms_dtb_stats, $
  ref_id = ref_id, $  ; Input keyword. Default is 'boxcar' if not specified.
  srf_id = srf_id     ; Input keyword. Default is ['table12','ngas'] if not specified.
;-
 
  @color_db
  @srf_parameters
  @error_codes


  ; Parameters
  N_PROFILES=83
  N_CHANNELS=22
  PROFILE_ID="ECMWF"+STRTRIM(N_PROFILES,2)

 
  ; Data set as reference
  IF ( N_ELEMENTS(ref_id) EQ 0 ) THEN $
    ref_dataset = 'boxcar' $
  ELSE $
    ref_dataset = ref_id[0]


  ; Data sets to view
  IF ( N_ELEMENTS(SRF_Id) EQ 0 ) THEN $
    dataset = ['table12','ngas'] $
  ELSE $
    dataset = SRF_Id
  n_datasets = N_ELEMENTS(dataset)


  ; Read the MonoRTM output
  ; ...The reference data first
  Tb_ref = DBLARR(N_PROFILES,N_CHANNELS)
  Read_Tb_Data, PROFILE_ID, "results/"+ref_dataset, Tb_ref
  ; ...All the other data
  Tb = DBLARR(N_PROFILES,N_CHANNELS)
  FOR i = 0, n_datasets-1 DO BEGIN
    Read_Tb_Data, PROFILE_ID, "results/"+dataset[i], Tb
    IF ( i EQ 0 ) THEN BEGIN
      Tb_dataset = Tb
      dTb = Tb - Tb_ref
    ENDIF ELSE BEGIN
      Tb_dataset = [[[Tb_dataset]], [[Tb]]]
      dTb = [[[dTb]], [[Tb - Tb_ref]]]
    ENDELSE
  ENDFOR
  
  
  ; Compute the stats and output
  FOR i = 0, n_datasets-1 DO BEGIN
    outfile = "results/"+dataset[i]+"/dTb.stats.dat"
    OPENW, fid, outfile, /GET_LUN, WIDTH=200
    PRINTF, fid, "! Ch#     AVG           SDEV"
    FOR l = 0, N_CHANNELS-1 DO BEGIN
      avg  = MEAN(dTb[*,l,i],/DOUBLE)
      sdev = STDDEV(dTb[*,l,i],/DOUBLE)
      PRINTF, fid, l+1, avg, sdev, FORMAT='(2x,i3,2(3x,f11.7))'
    ENDFOR
    FREE_LUN, fid
  ENDFOR
  
 
END ; PRO write_atms_dtb_stats.pro
