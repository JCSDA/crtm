FUNCTION RTS_Channel::Init, $
  Debug=Debug  ; Input keyword

  ; Set up
  COMPILE_OPT HIDDEN
  @rts_func_err_handler
  ; ...Local parameters
  ZERO = 0.0d0


  ; Set default values
  self.Is_Allocated = FALSE
  self.Sensor_Id         = BYTARR(20)
  self.WMO_Satellite_Id  = 0L
  self.WMO_Sensor_Id     = 0L
  self.Sensor_Channel    = 0L
  self.RT_Algorithm_Name = BYTARR(20)
  self.SOD                     = ZERO
  self.Surface_Emissivity      = ZERO
  self.Up_Radiance             = ZERO
  self.Down_Radiance           = ZERO
  self.Down_Solar_Radiance     = ZERO
  self.Surface_Planck_Radiance = ZERO
  self.Radiance                = ZERO
  self.Brightness_Temperature  = ZERO

  RETURN, TRUE

END
