
PRO TauProfile::InquireFile, $
   filename                         , $  ; Input
   Quiet           = quiet          , $  ; Input keyword
   Debug           = debug          , $  ; Input keyword
   n_Layers        = n_layers       , $  ; Output keyword
   n_Channels      = n_channels     , $  ; Output keyword
   n_Angles        = n_angles       , $  ; Output keyword
   n_Profiles      = n_profiles     , $  ; Output keyword
   n_Molecule_Sets = n_molecule_sets, $  ; Output keyword
   Sensor_ID       = sensor_id      , $  ; Output keyword
   Level_Pressure  = level_pressure , $  ; Output keyword
   Channel         = channel        , $  ; Output keyword
   Angle           = angle          , $  ; Output keyword
   Profile         = profile        , $  ; Output keyword
   Molecule_Set    = molecule_set        ; Output keyword

  ; Set up
  COMPILE_OPT HIDDEN
  @tauprofile_netcdf_parameters
  @tauprofile_pro_err_handler
  ; ...Check input
  IF ( ~ Valid_String(filename) ) THEN $
    MESSAGE, 'Must specify a filename', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ; ...Process keywords
  noisy = ~(KEYWORD_SET(quiet)) || KEYWORD_SET(debug)


  ; Open the file
  fid = NCDF_OPEN( filename, /NOWRITE )


  ; Read the global attributes
  NCDF_ATTGET, fid, SENSOR_ID_GATTNAME, sensor_id, /GLOBAL

  
  ; Read the dimensions
  dimid = NCDF_DIMID( fid, LAYER_DIMNAME )
  NCDF_DIMINQ, fid, dimid, dimname, n_layers
  dimid = NCDF_DIMID( fid, CHANNEL_DIMNAME )
  NCDF_DIMINQ, fid, dimid, dimname, n_channels
  dimid = NCDF_DIMID( fid, ANGLE_DIMNAME )
  NCDF_DIMINQ, fid, dimid, dimname, n_angles
  dimid = NCDF_DIMID( fid, PROFILE_DIMNAME )
  NCDF_DIMINQ, fid, dimid, dimname, n_profiles
  dimid = NCDF_DIMID( fid, MOLECULE_SET_DIMNAME )
  NCDF_DIMINQ, fid, dimid, dimname, n_molecule_sets


  ; Read the dimension list data
  varid = NCDF_VARID( fid, LEVEL_PRESSURE_VARNAME )
  NCDF_VARGET, fid, varid, level_pressure
  varid = NCDF_VARID( fid, CHANNEL_LIST_VARNAME )
  NCDF_VARGET, fid, varid, channel       
  varid = NCDF_VARID( fid, ANGLE_LIST_VARNAME )
  NCDF_VARGET, fid, varid, angle         
  varid = NCDF_VARID( fid, PROFILE_LIST_VARNAME )
  NCDF_VARGET, fid, varid, profile       
  varid = NCDF_VARID( fid, MOLECULE_SET_LIST_VARNAME )
  NCDF_VARGET, fid, varid, molecule_set  
  
  
  ; Done
  NCDF_CLOSE, fid

END
