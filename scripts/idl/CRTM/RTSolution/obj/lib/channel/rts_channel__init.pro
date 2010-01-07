FUNCTION RTS_Channel::Init, Debug=Debug  ; Input keyword

  ; Set up
  ; ...Local parameters
  ZERO = 0.0d0
  ; ...Set up error handler
  @rts_func_err_handler
 

  ; Set default values
  self.n_Allocates = 0L
  self.SOD                     = ZERO
  self.Surface_Emissivity      = ZERO
  self.Up_Radiance             = ZERO
  self.Down_Radiance           = ZERO
  self.Down_Solar_Radiance     = ZERO
  self.Surface_Planck_Radiance = ZERO
  self.Radiance                = ZERO
  self.Brightness_Temperature  = ZERO

  CATCH, /CANCEL
  RETURN, SUCCESS
 
END ; FUNCTION RTS_Channel::Init
