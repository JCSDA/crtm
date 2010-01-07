FUNCTION RTS_Channel::Set, $
  Debug                  =Debug                  , $  ; Input keyword
  SOD                    =SOD                    , $  ; Input keyword
  Surface_Emissivity     =Surface_Emissivity     , $  ; Input keyword
  Up_Radiance            =Up_Radiance            , $  ; Input keyword
  Down_Radiance          =Down_Radiance          , $  ; Input keyword
  Down_Solar_Radiance    =Down_Solar_Radiance    , $  ; Input keyword
  Surface_Planck_Radiance=Surface_Planck_Radiance, $  ; Input keyword
  Radiance               =Radiance               , $  ; Input keyword
  Brightness_Temperature =Brightness_Temperature , $  ; Input keyword
  Upwelling_Radiance     =Upwelling_Radiance     , $  ; Input keyword
  Layer_Optical_Depth    =Layer_Optical_Depth         ; Input keyword


  ; Set up
  ; ...Set up error handler
  @rts_func_err_handler
 
  
  ; Set scalar data
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

    IF ( N_ELEMENTS(Upwelling_Radiance) GT 0 ) THEN BEGIN
      IF ( N_ELEMENTS(Upwelling_Radiance) EQ self.n_Layers ) THEN $
        *self.Upwelling_Radiance = Upwelling_Radiance $
      ELSE $
        MESSAGE, 'Size of Upwelling_Radiance input inconsistent with n_Layers dimension.', $
                 NONAME=MsgSwitch, NOPRINT=MsgSwitch
    ENDIF

    IF ( N_ELEMENTS(Layer_Optical_Depth) GT 0 ) THEN BEGIN
      IF ( N_ELEMENTS(Layer_Optical_Depth) EQ self.n_Layers ) THEN $
        *self.Layer_Optical_Depth = Layer_Optical_Depth $
      ELSE $
        MESSAGE, 'Size of Layer_Optical_Depth input inconsistent with n_Layers dimension.', $
                 NONAME=MsgSwitch, NOPRINT=MsgSwitch
    ENDIF

  ENDIF

  
  ; Done
  CATCH, /CANCEL
  RETURN, SUCCESS
 
END ; FUNCTION RTS_Channel::Set
