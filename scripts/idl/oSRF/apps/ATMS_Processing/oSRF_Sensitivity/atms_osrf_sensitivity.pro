;+
; Driver script to compute the brightness temperature response
; of the various ATMS oSRF datasets for an AtmProfile dataset.
;
; MonoRTM generic input files must reside in a "generic_input/" subdirectory.
;
; Results are written to separate "results/<SRF_Id>" subdirectories with
; a filename cinvention of
;   <AtmProfile_Id>.Profile<XXXX>.Tb.dat
;
PRO atms_osrf_sensitivity, $
  Sensor_Id                    , $ ; Input
  SRF_Id                       , $ ; Input
  AtmProfile_Id                , $ ; Input
  Result_Path   = Result_Path  , $ ; Input keyword (Default = "results")
  Begin_Profile = Begin_Profile, $ ; Input keyword (Default = 1)
  End_Profile   = End_Profile  , $ ; Input keyword (Default = n_Profiles in AtmProfile)
  Channel_List  = Channel_List     ; Input keyword (Passed into Compute_ATMS_Tb)
;-

  ; Set up error handler
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN
  ENDIF
  
  
  ; Construct oSRF filenames
  SRF_File = Sensor_Id+"."+SRF_Id+".osrf.nc"
  n_SRF_Files = N_ELEMENTS(SRF_File)
  ; ...Check their existence
  FOR i = 0, n_SRF_Files -1 DO $
    IF ( NOT (FILE_INFO(SRF_File[i])).EXISTS ) THEN $
      MESSAGE, SRF_File[i]+" not found", /NONAME, /NOPRINT


  ; Determine the number of profiles
  AtmProfile_File = AtmProfile_Id+".AtmProfile.nc"
  atm_file = OBJ_NEW("AtmProfile_File", AtmProfile_File)
  atm_file->Read
  atm_file->Get_Property, n_Profiles = n_Profiles
  OBJ_DESTROY, atm_file
  

  ; Check results path
  IF ( Valid_String(Results_Path) ) THEN BEGIN
    r_path = Results_Path[0]
  ENDIF ELSE BEGIN
    r_path = "results"
  ENDELSE
  ; ...Create it, and its subdirs, if it doesn't already exist
  IF ( NOT FILE_TEST(r_path,/DIRECTORY) ) THEN FILE_MKDIR, r_path
  FOR i = 0, n_SRF_Files -1 DO BEGIN
    r_dir = r_path+PATH_SEP()+SRF_Id[i]
    IF ( NOT FILE_TEST(r_dir,/DIRECTORY) ) THEN FILE_MKDIR, r_dir
  ENDFOR
  
    
  ; Check profile limits
  IF ( KEYWORD_SET(Begin_Profile) ) THEN BEGIN
    _Begin_Profile = LONG(Begin_Profile[0])
    IF ( _Begin_Profile LT 0 OR _Begin_Profile GT n_Profiles ) THEN $
      MESSAGE, "Invalid begin profile index", /NONAME, /NOPRINT
  ENDIF ELSE BEGIN
    _Begin_Profile = 1
  ENDELSE
  IF ( KEYWORD_SET(End_Profile) ) THEN BEGIN
    _End_Profile = LONG(End_Profile[0])
    IF ( _End_Profile LT 0 OR _End_Profile GT n_Profiles ) THEN $
      MESSAGE, "Invalid end profile index", /NONAME, /NOPRINT
  ENDIF ELSE BEGIN
    _End_Profile = n_Profiles
  ENDELSE
  increment = ( _End_Profile LT _Begin_Profile ) ? -1 : 1

  
  ; Get the sensorinfo
  ; ...Determine if SensorInfo file is available
  SensorInfo_File = "SensorInfo"
  IF ( NOT (FILE_INFO(SensorInfo_File)).EXISTS ) THEN $
    MESSAGE, "SensorInfo file not found", /NONAME, /NOPRINT
  ; ...Read the list
  list = OBJ_NEW("SensorInfo_List", SensorInfo_File)
  list->Read
  ; ...Get the SensorInfo node for the required sensor
  si = list->Get(Sensor_Id=Sensor_Id) 
  ; ...Get the channel list
  si->Get_Property, Sensor_Channel = Sensor_Channel  
  n_Channels = N_ELEMENTS(Sensor_Channel)
  ; ...Cleanup
  OBJ_DESTROY, list


  ; Check Channel limits
  IF ( N_ELEMENTS(Channel_List) GT 0 ) THEN BEGIN
    Sensor_Channel = LONG(Channel_List)
    n_Channels = N_ELEMENTS(Sensor_Channel)
  ENDIF


  ; Loop over profiles
  FOR m = _Begin_Profile, _End_Profile, increment DO BEGIN

    PRINT, FORMAT='(/5x,"Processing ",a," profile #:",i5)', AtmProfile_Id, m
    
    ; Construct the generic MonoRTM input filenames
    MonoRTM_Infile = "generic_input/TAPE5."+AtmProfile_Id+"_profile"+STRING(FORMAT='(i4.4)',m)

    ; Loop over SRF data sources
    FOR i = 0, n_SRF_Files - 1 DO BEGIN

      PRINT, FORMAT='(7x,"Using ",a," SRF data")', SRF_Id[i]

      ; Compute the ATMS brightness temperatures
      time = SYSTIME(1)
      Tb = Compute_ATMS_Tb( Sensor_Id, $
                            MonoRTM_Infile , $
                            SRF_File[i], $
                            Channel_List = Sensor_Channel )
      PRINT, FORMAT='(9x,"Time: ",f5.1,"min.")', (SYSTIME(1)-time)/60.0d0
      
      ; Write the data to file
      Tb_Outfile = r_path+PATH_SEP()+SRF_Id[i]+PATH_SEP()+ $
                   AtmProfile_Id+".Profile"+STRING(m,FORMAT='(i4.4)')+".Tb.dat"
      ; ...Slot in current data if the datafile already exists
      IF ( FILE_TEST(Tb_Outfile) ) THEN BEGIN
        read_atms_Tb, Tb_Outfile, xTb, Channel_List = xCL
        FOR l = 0, n_Channels - 1 DO BEGIN
          ; ...Does the current channel data already exist?
          loc = WHERE( Sensor_Channel[l] EQ xCL, count )
          IF ( count GT 0 ) THEN BEGIN
            xTb[loc[0]] = Tb[l]    ; Yes. Replace it
          ENDIF ELSE BEGIN
            xTb = [xTb, Tb[l] ]    ; No.  Add it.
            xCL = [xCl, Sensor_Channel[l]]
          ENDELSE
        ENDFOR
      ENDIF ELSE BEGIN
        xTb = Tb
        xCL = Sensor_Channel
      ENDELSE
      ; ...Write data
      write_atms_Tb, Tb_Outfile, xTb, Channel_List = xCL

    ENDFOR

  ENDFOR

END
