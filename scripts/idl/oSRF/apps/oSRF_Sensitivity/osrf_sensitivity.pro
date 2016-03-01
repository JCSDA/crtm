;+
; Script to compute the brightness temperature response
; of various oSRF datasets for an AtmProfile dataset.
;
; MonoRTM generic input files must reside in a "generic_input/" subdirectory
; with a filename convention of
;   TAPE5.<AtmProfile_Id>.profile<XXXX>
;
; Results are written to separate "results/<SRF_Id>" subdirectories with
; a filename convention of
;   <AtmProfile_Id>.profile<XXXX>.Tb.dat
;
PRO osrf_sensitivity, $
  Sensor_Id                        , $ ; Input
  SRF_Id                           , $ ; Input
  SensorInfo_File = sensorinfo_file, $ ; Input keyword (Default = "SensorInfo")
  Result_Path     = result_path    , $ ; Input keyword (Default = "results")
  Begin_Profile   = begin_profile  , $ ; Input keyword (Default = 1. Ignored if Profile_List set)
  End_Profile     = end_profile    , $ ; Input keyword (Default = n_Profiles. Ignored if Profile_List set)
  Profile_List    = profile_list   , $ ; Input keyword (Default = all profiles)
  Channel_List    = channel_list   , $ ; Input keyword (Passed into oSRF_Compute_Tb)
  Debug           = debug
;-

  ; Setup
  COMPILE_OPT HIDDEN
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler
  ; ...Set local parameters
  INPUT_PATH = "generic_input"
  ; ...Check keywords
  sinfo_file = Valid_String(sensorinfo_file) ? sensorinfo_file[0] : "SensorInfo"
  r_path     = Valid_String(result_path)     ? result_path[0]     : "results"

  
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
    Debug            = debug           , $
    Sensor_Name      = sensor_name     , $
    Satellite_Name   = satellite_name  , $
    WMO_Satellite_ID = wmo_satellite_id, $
    WMO_Sensor_ID    = wmo_sensor_id   , $
    Sensor_Type      = sensor_type     , $
    Sensor_Channel   = sensor_channel
  ; ...Set sone sensor info
  n_channels   = N_ELEMENTS(sensor_channel)
  is_microwave = (sensor_type EQ MICROWAVE_SENSOR )


  ; Construct oSRF filenames
  SRF_File = Sensor_Id+"."+SRF_Id+".osrf.nc"
  n_srf_files = N_ELEMENTS(SRF_File)
  ; ...Check their existence
  FOR i = 0, n_srf_files - 1 DO $
    IF ( ~ (FILE_INFO(SRF_File[i])).EXISTS ) THEN $
      MESSAGE, SRF_File[i]+" not found", $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Create it, and its subdirs, if it doesn't already exist
  IF ( ~ FILE_TEST(r_path,/DIRECTORY) ) THEN FILE_MKDIR, r_path
  FOR i = 0, n_srf_files - 1 DO BEGIN
    r_dir = r_path+PATH_SEP()+SRF_Id[i]
    IF ( NOT FILE_TEST(r_dir,/DIRECTORY) ) THEN FILE_MKDIR, r_dir
  ENDFOR
  
    
  ; Determine the number of profiles
  input_files = FILE_SEARCH(INPUT_PATH+PATH_SEP()+"TAPE5.*", COUNT=n_profiles)
  IF ( n_profiles EQ 0 ) THEN $
    MESSAGE, "No input TAPE5 files found", $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ; ...Extract the AtmProfile id from the filenames
  atmprofile_id = (STRSPLIT(FILE_BASENAME(input_files[0]), '.', /EXTRACT))[1]


  ; Check the profile limits
  _begin_profile = 1
  IF ( N_ELEMENTS(begin_profile) GT 0 ) THEN BEGIN
    _begin_profile = LONG(begin_profile[0])
    IF ( (_begin_profile LT 1) OR (_begin_profile GT n_profiles) ) THEN $
      MESSAGE, "Invalid begin profile index", $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ENDIF
  _end_profile = n_profiles
  IF ( N_ELEMENTS(end_profile) GT 0 ) THEN BEGIN
    _end_profile = LONG(end_profile[0])
    IF ( (_end_profile LT _begin_profile) OR (_end_profile GT n_profiles) ) THEN $
      MESSAGE, "Invalid end profile index", $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ENDIF
  _n_profiles   = _end_profile - _begin_profile + 1
  _profile_list = LINDGEN(_n_profiles) + _begin_profile


  ; Check the profile list
  IF ( N_ELEMENTS(profile_list) GT 0 ) THEN BEGIN
    _profile_list  = LONG(profile_list)
    _begin_profile = MIN(_profile_list)
    _end_profile   = MAX(_profile_list)
    IF ( (_begin_profile LT 1) OR (_begin_profile GT n_profiles) ) THEN $
      MESSAGE, "Invalid begin profile in Profile_List", $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    IF ( (_end_profile LT _begin_profile) OR (_end_profile GT n_profiles) ) THEN $
      MESSAGE, "Invalid end profile in Profile_List", $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    _n_profiles = N_ELEMENTS(_profile_list)
  ENDIF

  
  ; Get the channel list
  IF ( N_ELEMENTS(channel_list) GT 0 ) THEN $
    _channel_list = LONG(channel_list) $
  ELSE $
    _channel_list = sensor_channel
  ; ...and check it for invalid channels
  FOR l = 0, N_ELEMENTS(_channel_list)-1 DO BEGIN
    idx = WHERE(sensor_channel EQ _channel_list[l], count)
    IF ( count EQ 0 ) THEN $
      MESSAGE, "Invalid channel for "+STRTRIM(Sensor_Id,2)+" specified in channel list", $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ENDFOR
  n_channels = N_ELEMENTS(_channel_list)


  ; Loop over profiles
  FOR m = 0L, _n_profiles - 1L DO BEGIN

    PRINT, FORMAT='(/5x,"Processing profile #:",i5)', _profile_list[m]

    
    ; Construct the generic LBL input filenames
    c_profile = STRING(FORMAT='(i4.4)',_profile_list[m])
    lbl_infile = "generic_input/TAPE5."+atmprofile_id+".profile"+c_profile


    ; Loop over SRF data sources
    FOR i = 0, n_SRF_Files - 1 DO BEGIN

      PRINT, FORMAT='(7x,"Using ",a," SRF data")', SRF_Id[i]


      ; Compute the brightness temperatures
      time = SYSTIME(1)
      tb = oSRF_Compute_Tb( sinfo, $
                            lbl_infile , $
                            SRF_File[i], $
                            Channel_List = _channel_list, $
                            Debug = debug )
      PRINT, FORMAT='(9x,"Time: ",f5.1,"min.")', (SYSTIME(1)-time)/60.0d0

      
      ; Write the data to file
      tb_outfile = r_path+PATH_SEP()+SRF_Id[i]+PATH_SEP()+ $
                   atmprofile_id+".profile"+c_profile+".Tb.dat"
      ; ...Slot in current data if the datafile already exists
      IF ( FILE_TEST(tb_outfile) ) THEN BEGIN
        oSRF_Read_Tb, tb_outfile, xtb, Channel_List = xcl
        FOR l = 0, n_channels - 1 DO BEGIN
          ; ...Does the current channel data already exist?
          loc = WHERE( _channel_list[l] EQ xcl, count )
          IF ( count GT 0 ) THEN BEGIN
            xtb[loc[0]] = tb[l]    ; Yes. Replace it
          ENDIF ELSE BEGIN
            xtb = [xtb, tb[l] ]    ; No.  Add it.
            xcl = [xcl, _channel_list[l]]
          ENDELSE
        ENDFOR
      ENDIF ELSE BEGIN
        xtb = tb
        xcl = _channel_list
      ENDELSE
      ; ...Write data
      oSRF_Write_Tb, tb_outfile, xtb, Channel_List = xcl

    ENDFOR  ; SRF dataset loop

  ENDFOR  ; Profile loop

END
