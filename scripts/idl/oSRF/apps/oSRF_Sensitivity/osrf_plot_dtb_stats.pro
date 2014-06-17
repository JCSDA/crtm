PRO Read_All_Tb_Data, atmprofile_id, $  ; Input
                      path         , $  ; Input
                      tb           , $  ; Output
                      Debug = debug
  ; Setup
  COMPILE_OPT HIDDEN

  ; Get info
  info = SIZE(tb,/STRUCT)
  ; Read the profile data results
  FOR m = 0, info.DIMENSIONS[0]-1 DO BEGIN
    filename = path+"/"+atmprofile_id+'.profile'+STRING(m+1,FORMAT='(i4.4)')+'.Tb.dat'
    oSRF_Read_Tb, filename, x, Debug = debug
    tb[m,*] = x
  ENDFOR
END


;+
; Procedure to display the oSRF dTb residual data statistics.
;
PRO oSRF_Plot_dTb_Stats, $
  Sensor_Id                        , $  ; Input
  AtmProfile_Id                    , $  ; Input
  SensorInfo_File = sensorinfo_file, $  ; Input keyword. (Default is "SensorInfo")
  Result_Path     = result_path    , $  ; Input keyword. (Default is "results")
  Ref_Id          = ref_id         , $  ; Input keyword. (Default is "reference")
  Exp_Id          = exp_id         , $  ; Input keyword. (Default is "experiment"). Can be an array.       
  Legend_Name     = legend_name    , $  ; Input keyword. (Default is Exp_Id)                 
  EPS             = eps            , $  ; Input keyword. Set to output EPS plot. Default is PNG.                   
  PlotRef         = plotref        , $  ; Output keyword. Object plot reference.
  Debug           = debug
;-
 
  ; Setup
  COMPILE_OPT HIDDEN
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler
  ; ...Check keywords
  sinfo_file  = Valid_String(sensorinfo_file) ? sensorinfo_file[0] : "SensorInfo"
  r_path      = Valid_String(result_path)     ? result_path[0]     : "results"
  ref_dataset = (N_ELEMENTS(ref_id) GT 0)     ? ref_id[0]          : 'reference'
  exp_dataset = (N_ELEMENTS(exp_id) GT 0)     ? exp_id             : ['experiment']
  n_exp_datasets = N_ELEMENTS(exp_dataset)
  ; ...Create the legend name array
  IF ( N_ELEMENTS(legend_name) EQ n_exp_datasets ) THEN $
    exp_dataset_name = STRING(legend_name) $
  ELSE $
    exp_dataset_name = exp_dataset

  
  ; Statistics to plot
  stat_name = ['Average','Std.Dev.','Min. value','Max. value']
  n_stats = N_ELEMENTS(stat_name)
  nx = 2
  ny = 2
  
  
  ; Get the sensor information
  ; ...Read the SensorInfo file
  sinfo_list = SensorInfo_List(sinfo_file)
  sinfo_list->Read, Debug=debug
  ; ...Get the sensor entry
  sinfo = sinfo_list->Get(Sensor_Id=Sensor_Id,COUNT=count,Debug=debug)
  IF ( count NE 1 ) THEN $
    MESSAGE, Sensor_Id+' entry not found in '+STRTRIM(sinfo_file,2), $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ; ...Extract the sensor properties required
  sinfo->Get_Property, $
    Debug          = debug         , $
    Sensor_Type    = sensor_type   , $
    Sensor_Channel = sensor_channel
  n_channels = N_ELEMENTS(sensor_channel)
  
    
  ; Determine the number of profiles
  ref_files = FILE_SEARCH(r_path+PATH_SEP()+ref_dataset+PATH_SEP()+"*.dat", COUNT=n_profiles)
  IF ( n_profiles EQ 0 ) THEN $
    MESSAGE, "No reference dataset files found!", $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ; ...Extract the AtmProfile id from the filenames
  atmprofile_id = (STRSPLIT(FILE_BASENAME(ref_files[0]), '.', /EXTRACT))[0]


  ; Graphics info
  ; ...Bar colours
  bar_colour = ['green', 'red', 'blue', 'orange', 'magenta','cyan']
  IF ( n_exp_datasets GT N_ELEMENTS(bar_colour) ) THEN $
    MESSAGE, "Too many experimental datasets!", $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ; ...Font sizes for plots
  font_size   = 9
  l_font_size = 6
  IF ( KEYWORD_SET(eps) ) THEN BEGIN
    font_size   = font_size   * 1.5
    l_font_size = l_font_size * 1.5
  ENDIF


  ; Read all LBL output
  ; ...The reference data first
  ref_tb = DBLARR(n_profiles, n_channels)
  Read_All_Tb_Data, atmprofile_id, r_path+PATH_SEP()+ref_dataset, ref_tb, Debug=debug
  ; ...All the other data
  exp_tb = DBLARR(n_profiles, n_channels)
  FOR i = 0, n_exp_datasets-1 DO BEGIN
    Read_All_Tb_Data, atmprofile_id, r_path+PATH_SEP()+exp_dataset[i], exp_tb, Debug=debug
    IF ( i EQ 0 ) THEN $
      dtb = exp_tb - ref_tb $
    ELSE $
      dtb = [[[dtb]], [[exp_tb - ref_tb]]]
  ENDFOR
  
  
  ; Compute the stats
  stats = DBLARR(n_channels, n_exp_datasets, n_stats)
  FOR i = 0, n_exp_datasets-1 DO BEGIN
    FOR l = 0, n_channels-1 DO BEGIN
      stats[l,i,0] = MEAN(dtb[*,l,i],/DOUBLE)
      stats[l,i,1] = STDDEV(dtb[*,l,i],/DOUBLE)
      stats[l,i,2] = MIN(dtb[*,l,i])
      stats[l,i,3] = MAX(dtb[*,l,i])
    ENDFOR
  ENDFOR

 
  ; Create a barplot of the stats
  w = WINDOW(WINDOW_TITLE=STRTRIM(Sensor_Id,2)+'dTb statistics')
  ; ...Plot the statistics
  FOR j = 0, n_stats-1 DO BEGIN
    index = 0
    FOR i = 0, n_exp_datasets-1 DO BEGIN
      b = BARPLOT( INDGEN(n_channels)+1, stats[*,i,j], $
                   NBARS=n_exp_datasets, $
                   INDEX=index++, $
                   FILL_COLOR=bar_colour[i], $
                   FONT_SIZE=font_size, $
                   TITLE=stat_name[j], $
                   XTITLE='Channel', $
                   YTITLE=stat_name[j]+' !9D!XT!DB!N', $
                   NAME = exp_dataset_name[i], $
                   OVERPLOT=i, $
                   LAYOUT = [nx,ny,j+1], $
                   MARGIN = [0.25,0.16,0.05,0.1], $
                   /CURRENT, $
                   XTICKINTERVAL=1, $
                   XMINOR=0, $
                   XTICKVALUES=INDGEN(n_channels),$
                   XTICKNAME=SINDGEN(n_channels, START=1) )
      IF ( j EQ 0 AND n_exp_datasets GT 1 ) THEN BEGIN
        IF ( i EQ 0 ) THEN $
          l = LEGEND( TARGET=b, $
                      POSITION=[0.8,0.925], $
                      FONT_SIZE=l_font_size ) $
        ELSE BEGIN
          l.Add, b
        ENDELSE
      ENDIF
    ENDFOR
  ENDFOR
  
  IF ( KEYWORD_SET(eps) ) THEN BEGIN
    w.Save, STRTRIM(Sensor_Id,2)+".dTb_stats.eps", BORDER=10
  ENDIF ELSE BEGIN
    w.Save, STRTRIM(Sensor_Id,2)+".dTb_stats.png", HEIGHT=500, BORDER=10
  ENDELSE

  plotref = w
END
