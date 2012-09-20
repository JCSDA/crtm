;+
PRO RTS_File::Read, $
  Filename, $  ; Input
  Debug=Debug    ; Input keyword
;-

  ; Set up
  @rts_pro_err_handler


  ; Process input
  IF ( NOT Valid_String(Filename) ) THEN $
    MESSAGE, 'Must specify a filename', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  self.filename = Filename


  ; Check the file exists
  fInfo = FILE_INFO(self.filename)
  IF ( NOT fInfo.EXISTS ) THEN $
    MESSAGE, self.filename+' not found', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Open the file for reading
  fileid = Open_Binary_File( self.filename )
  IF ( fileid < 0 ) THEN $
    MESSAGE, 'Error opening file '+self.filename, $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch

  ; Read the file dimensions
  n_channels = 0L
  n_profiles = 0L
  READU, fileid, n_channels, n_profiles
  IF ( n_profiles EQ 0 ) THEN n_profiles = 1L
  self.n_profiles = n_profiles


  ; Loop over the number of profiles
  FOR m = 0L, n_profiles - 1L DO BEGIN


    ; Create the profile container
    rts_profile = OBJ_NEW('RTS_Profile');, Debug = Debug )


    ; Loop over the number of channels
    FOR n = 0L, n_channels - 1L DO BEGIN

      ; Read the data dimension
      n_layers = 0L
      READU, fileid, n_layers

      ; Create the current RTS channel object
      rts_channel = OBJ_NEW('RTS_Channel');, Debug = Debug )
      ; ...Allocate if we have layer data
      IF ( n_layers GT 0 ) THEN rts_channel->Create, n_layers, Debug = Debug

      ; Read the sensor info
      rts_channel->RTS_Channel::Get_Property, $
        Debug = Debug, $
        Sensor_Id        = sensor_id       , $
        WMO_Satellite_Id = wmo_satellite_id, $
        WMO_Sensor_Id    = wmo_sensor_id   , $
        Sensor_Channel   = sensor_channel
      READU, fileid, $
             sensor_id       , $
             wmo_satellite_id, $
             wmo_sensor_id   , $
             sensor_channel
      rts_channel->RTS_Channel::Set_Property, $
        Debug = Debug, $
        Sensor_Id        = sensor_id       , $
        WMO_Satellite_Id = wmo_satellite_id, $
        WMO_Sensor_Id    = wmo_sensor_id   , $
        Sensor_Channel   = sensor_channel


      ; Read the RT algorithm name
      rts_channel->RTS_Channel::Get_Property, $
        Debug = Debug, $
        RT_Algorithm_Name = rt_algorithm_name
      READU, fileid, $
             rt_algorithm_name
      rts_channel->RTS_Channel::Set_Property, $
        Debug = Debug, $
        RT_Algorithm_Name = rt_algorithm_name


      ; Read the forward radiative transfer intermediate results
      ; ... Scalars
      rts_channel->RTS_Channel::Get_Property, $
        Debug = Debug, $
        SOD                     = SOD                    , $
        Surface_Emissivity      = Surface_Emissivity     , $
        Up_Radiance             = Up_Radiance            , $
        Down_Radiance           = Down_Radiance          , $
        Down_Solar_Radiance     = Down_Solar_Radiance    , $
        Surface_Planck_Radiance = Surface_Planck_Radiance
      READU, fileid, $
             SOD                    , $
             Surface_Emissivity     , $
             Up_Radiance            , $
             Down_Radiance          , $
             Down_Solar_Radiance    , $
             Surface_Planck_Radiance
      rts_channel->RTS_Channel::Set_Property, $
        Debug = Debug, $
        SOD                     = SOD                    , $
        Surface_Emissivity      = Surface_Emissivity     , $
        Up_Radiance             = Up_Radiance            , $
        Down_Radiance           = Down_Radiance          , $
        Down_Solar_Radiance     = Down_Solar_Radiance    , $
        Surface_Planck_Radiance = Surface_Planck_Radiance
      ; ...Arrays
      IF ( n_layers GT 0 ) THEN BEGIN
        rts_channel->RTS_Channel::Get_Property, $
          Debug = Debug, $
          Upwelling_Radiance  = Upwelling_Radiance , $
          Layer_Optical_Depth = Layer_Optical_Depth
        READU, fileid, $
               Upwelling_Radiance , $
               Layer_Optical_Depth
        rts_channel->RTS_Channel::Set_Property, $
          Debug = Debug, $
          Upwelling_Radiance  = Upwelling_Radiance , $
          Layer_Optical_Depth = Layer_Optical_Depth
      ENDIF
      ; ... The radiative transfer results
      rts_channel->RTS_Channel::Get_Property, $
        Debug = Debug, $
        Radiance               = Radiance              , $
        Brightness_Temperature = Brightness_Temperature
      READU, fileid, $
             Radiance              , $
             Brightness_Temperature
      rts_channel->RTS_Channel::Set_Property, $
        Debug = Debug, $
        Radiance               = Radiance              , $
        Brightness_Temperature = Brightness_Temperature


      ; Add the current RTS_Channel to the profile object
      rts_profile->Add, rts_channel


    ENDFOR


    ; Add the current RTS_Profile to the file object
    self->Add, rts_profile


  ENDFOR
  FREE_LUN, fileid

END ; PRO RTS_File::Read
