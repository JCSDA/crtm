PRO Create_IASI_SensorInfo
  @iasi_parameters
  
  Sensor_Name    = 'IASI'
  Satellite_Name = 'MetOp-A'
  WMO_Sensor_Id    = 221
  WMO_Satellite_Id = 4
  Microwave_Flag = 0
  
  Use_Flag = 1
  Noise_Level = 100.0
  
  OPENW, FileID, 'IASI.SensorInfo', /GET_LUN
  FOR i = 1, N_BANDS DO BEGIN
    Channels = IASI_Channels(i)
    n_Channels = N_ELEMENTS(Channels)
    
    Sensor_Id = 'iasiB'+STRTRIM(i,2)+'_metopa'
    PRINTF, FileID, Sensor_Name, Satellite_Name, Sensor_Id, Microwave_Flag, $
                    WMO_Sensor_Id, WMO_Satellite_Id, n_Channels, $
                    FORMAT='(1x,2(1x,a-12),1x,a-20,1x,i1,6x,3(1x,i5))'
    FOR l = 0L, n_Channels-1L DO BEGIN
      PRINTF, FileID, Channels[l], Use_Flag, Noise_Level, $
                      FORMAT='(i5,3x,i2,5x,e13.6)'
    ENDFOR
  ENDFOR
  FREE_LUN, FileID

END
