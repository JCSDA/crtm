FUNCTION RTS_Channel::Assign, $
  new, $       ; Output
  Debug=Debug  ; Input keyword

  ; Set up
  ; ...Set up error handler
  @rts_func_err_handler

  ; Create a new object reference
  ; ...Destroy output object if defined.
  IF ( N_ELEMENTS(new) GT 0 ) THEN $
    IF ( SIZE(new, /TNAME) EQ 'OBJREF' ) THEN OBJ_DESTROY, new, Debug=Debug

  ; ...Create a new object reference
  new = OBJ_NEW('RTS_Channel',Debug=Debug)
  

  ; Assign array data
  IF ( self->Associated(Debug=Debug) ) THEN BEGIN
  
    ; Allocate the output object
    Result = new->Allocate( self.n_Layers,Debug=Debug )
    IF ( Result NE SUCCESS ) THEN $
      MESSAGE, 'Error allocating output RTS structure', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch

    ; Copy over data
    *new.Upwelling_Radiance  = *self.Upwelling_Radiance  
    *new.Layer_Optical_Depth = *self.Layer_Optical_Depth 
  ENDIF
  
  
  ; Assign scalar data
  new.SOD                     = self.SOD                    
  new.Surface_Emissivity      = self.Surface_Emissivity     
  new.Up_Radiance             = self.Up_Radiance            
  new.Down_Radiance           = self.Down_Radiance          
  new.Down_Solar_Radiance     = self.Down_Solar_Radiance    
  new.Surface_Planck_Radiance = self.Surface_Planck_Radiance
  new.Radiance                = self.Radiance               
  new.Brightness_Temperature  = self.Brightness_Temperature 


  ; Done
  CATCH, /CANCEL
  RETURN, SUCCESS

END ; FUNCTION RTS_Channel::Assign
