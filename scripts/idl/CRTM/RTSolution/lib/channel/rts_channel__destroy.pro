;+
PRO RTS_Channel::Destroy, $
  Debug=Debug           ; Input keyword
;-
  ; Set up
  @rts_pro_err_handler
  ; ...Local parameters
  ZERO = 0.0d0


  ; Reinitialise
  self.Is_Allocated = FALSE
  self.n_Layers = 0L
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
  PTR_FREE, self.Upwelling_Radiance
  PTR_FREE, self.Layer_Optical_Depth

END
