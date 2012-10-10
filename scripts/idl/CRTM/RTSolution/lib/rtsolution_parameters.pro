  ; Length of string components
  SENSOR_ID_STRLEN = 20
  ALGORITHM_STRLEN = 20
  
  ; Literal constant definitions
  ZERO = 0.0d0
  
  ; The channel data that can be displayed
  CHANNEL_DATA_NAME = [ $
    'Surface_Emissivity'     , $
    'SOD'                    , $
    'Up_Radiance'            , $
    'Down_Radiance'          , $
    'Down_Solar_Radiance'    , $
    'Surface_Planck_Radiance', $
    'Radiance'               , $
    'Brightness_Temperature'   ]
  N_CHANNEL_DATA_ITEMS = N_ELEMENTS(CHANNEL_DATA_NAME)

