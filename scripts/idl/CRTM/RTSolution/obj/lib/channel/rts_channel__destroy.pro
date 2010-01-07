FUNCTION RTS_Channel::Destroy, $
  No_Clear=No_Clear, $  ; Input keyword
  Debug=Debug           ; Input keyword
 
  ; Set up
  ; ...Local parameters
  ZERO = 0.0d0
  ; ...Set up error handler
  @rts_func_err_handler
 
 
  ; Reinitialise the dimensions
  self.n_Layers = 0L

  
  ; Initialise the scalar members
  IF ( NOT KEYWORD_SET(No_Clear) ) THEN BEGIN
    self.SOD                     = ZERO
    self.Surface_Emissivity      = ZERO
    self.Up_Radiance             = ZERO
    self.Down_Radiance           = ZERO
    self.Down_Solar_Radiance     = ZERO
    self.Surface_Planck_Radiance = ZERO
    self.Radiance                = ZERO
    self.Brightness_Temperature  = ZERO
  ENDIF


  ; If ALL pointer members are NOT associated, do nothing
  IF ( self->Associated(Debug=Debug) EQ FALSE ) THEN GOTO, Done


  ; Deallocate the pointer members and nullify
  PTR_FREE, self.Upwelling_Radiance , $
            self.Layer_Optical_Depth
  self.Upwelling_Radiance  = PTR_NEW()
  self.Layer_Optical_Depth = PTR_NEW()

  ; Decrement and test allocation counter
  self.n_Allocates = self.n_Allocates - 1
  IF ( self.n_Allocates NE 0 ) THEN $
    MESSAGE, 'Allocation counter /= 0, Value = ' + STRTRIM(self.n_Allocates, 2), $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch

  ; Done
  Done:
  CATCH, /CANCEL
  RETURN, SUCCESS

END ; FUNCTION RTS_Channel::Destroy
