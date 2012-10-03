; Helper procedure
PRO Assign_Arrays, name, input_array, object_array, Debug=debug
  COMPILE_OPT HIDDEN
  @rts_pro_err_handler
  input_info = SIZE(input_array,/STRUCTURE)
  object_info = SIZE(object_array,/STRUCTURE)
  IF ( input_info.N_DIMENSIONS NE object_info.N_DIMENSIONS ) THEN $
    MESSAGE, 'Number of '+STRTRIM(name,2)+' input array dimensions is different from object definition.', $
        NONAME=MsgSwitch, NOPRINT=MsgSwitch
  FOR n = 0, input_info.N_DIMENSIONS - 1 DO BEGIN
    IF ( input_info.DIMENSIONS[n] NE object_info.DIMENSIONS[n] ) THEN $
      MESSAGE, 'Size of input '+STRTRIM(name,2)+' array is different from that allocated.', $
          NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ENDFOR
  object_array = input_array
END

;+
PRO RTS_Channel::Set_Property, $
  Debug                   = Debug                  , $  ; Input keyword
  Sensor_Id               = Sensor_Id              , $  ; Input keyword
  WMO_Satellite_Id        = WMO_Satellite_Id       , $  ; Input keyword
  WMO_Sensor_Id           = WMO_Sensor_Id          , $  ; Input keyword
  Sensor_Channel          = Sensor_Channel         , $  ; Input keyword
  RT_Algorithm_Name       = RT_Algorithm_Name      , $  ; Input keyword
  SOD                     = SOD                    , $  ; Input keyword
  Surface_Emissivity      = Surface_Emissivity     , $  ; Input keyword
  Up_Radiance             = Up_Radiance            , $  ; Input keyword
  Down_Radiance           = Down_Radiance          , $  ; Input keyword
  Down_Solar_Radiance     = Down_Solar_Radiance    , $  ; Input keyword
  Surface_Planck_Radiance = Surface_Planck_Radiance, $  ; Input keyword
  Radiance                = Radiance               , $  ; Input keyword
  Brightness_Temperature  = Brightness_Temperature , $  ; Input keyword
  Upwelling_Radiance      = Upwelling_Radiance     , $  ; Input keyword
  Layer_Optical_Depth     = Layer_Optical_Depth         ; Input keyword
;-

  ; Set up
  @rts_pro_err_handler


  ; Set scalar data
  IF ( N_ELEMENTS(Sensor_Id              ) GT 0 ) THEN self.Sensor_Id               = Sensor_Id
  IF ( N_ELEMENTS(WMO_Satellite_Id       ) GT 0 ) THEN self.WMO_Satellite_Id        = WMO_Satellite_Id
  IF ( N_ELEMENTS(WMO_Sensor_Id          ) GT 0 ) THEN self.WMO_Sensor_Id           = WMO_Sensor_Id
  IF ( N_ELEMENTS(Sensor_Channel         ) GT 0 ) THEN self.Sensor_Channel          = Sensor_Channel
  IF ( N_ELEMENTS(RT_Algorithm_Name      ) GT 0 ) THEN self.RT_Algorithm_Name       = RT_Algorithm_Name
  IF ( N_ELEMENTS(SOD                    ) GT 0 ) THEN self.SOD                     = SOD
  IF ( N_ELEMENTS(Surface_Emissivity     ) GT 0 ) THEN self.Surface_Emissivity      = Surface_Emissivity
  IF ( N_ELEMENTS(Up_Radiance            ) GT 0 ) THEN self.Up_Radiance             = Up_Radiance
  IF ( N_ELEMENTS(Down_Radiance          ) GT 0 ) THEN self.Down_Radiance           = Down_Radiance
  IF ( N_ELEMENTS(Down_Solar_Radiance    ) GT 0 ) THEN self.Down_Solar_Radiance     = Down_Solar_Radiance
  IF ( N_ELEMENTS(Surface_Planck_Radiance) GT 0 ) THEN self.Surface_Planck_Radiance = Surface_Planck_Radiance
  IF ( N_ELEMENTS(Radiance               ) GT 0 ) THEN self.Radiance                = Radiance
  IF ( N_ELEMENTS(Brightness_Temperature ) GT 0 ) THEN self.Brightness_Temperature  = Brightness_Temperature


  ; Set array data
  IF ( self->Associated(Debug=Debug) ) THEN BEGIN
    IF ( N_ELEMENTS(Upwelling_Radiance) GT 0 ) THEN $
      Assign_Arrays, 'Upwelling_Radiance', Upwelling_Radiance, *self.Upwelling_Radiance,/STRUCTURE, Debug=debug
    IF ( N_ELEMENTS(Layer_Optical_Depth) GT 0 ) THEN $
      Assign_Arrays, 'Layer_Optical_Depth', Layer_Optical_Depth, *self.Layer_Optical_Depth,/STRUCTURE, Debug=debug
  ENDIF

END
