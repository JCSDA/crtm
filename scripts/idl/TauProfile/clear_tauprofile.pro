;-------------------------------
PRO Clear_TauProfile, TauProfile  ; Output
;-------------------------------
  @tauprofile_netcdf_parameters
  TauProfile.Release = TAUPROFILE_RELEASE
  TauProfile.Version = TAUPROFILE_VERSION
  TauProfile.Sensor_ID        = ' '
  TauProfile.WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID
  TauProfile.WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID   
END ; PRO Clear_TauProfile
