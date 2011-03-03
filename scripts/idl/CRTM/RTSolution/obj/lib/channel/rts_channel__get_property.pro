PRO RTS_Channel::Get_Property, $
  Debug                  =Debug                  , $  ; Input keyword
  SOD                    =SOD                    , $  ; Output keyword
  Surface_Emissivity     =Surface_Emissivity     , $  ; Output keyword
  Up_Radiance            =Up_Radiance            , $  ; Output keyword
  Down_Radiance          =Down_Radiance          , $  ; Output keyword
  Down_Solar_Radiance    =Down_Solar_Radiance    , $  ; Output keyword
  Surface_Planck_Radiance=Surface_Planck_Radiance, $  ; Output keyword
  Radiance               =Radiance               , $  ; Output keyword
  Brightness_Temperature =Brightness_Temperature , $  ; Output keyword
  Upwelling_Radiance     =Upwelling_Radiance     , $  ; Output keyword
  Layer_Optical_Depth    =Layer_Optical_Depth         ; Output keyword


  ; Set up
  ; ...Local parameters
  INVALID = -1.0d0
  ; ...Set up error handler
  @rts_pro_err_handler
 
  
  ; Get scalar data
  SOD                     = self.SOD                    
  Surface_Emissivity      = self.Surface_Emissivity     
  Up_Radiance             = self.Up_Radiance            
  Down_Radiance           = self.Down_Radiance          
  Down_Solar_Radiance     = self.Down_Solar_Radiance    
  Surface_Planck_Radiance = self.Surface_Planck_Radiance
  Radiance                = self.Radiance               
  Brightness_Temperature  = self.Brightness_Temperature 

  ; Get array data
  IF ( self->Associated(Debug=Debug) ) THEN BEGIN
    Upwelling_Radiance  = *self.Upwelling_Radiance  
    Layer_Optical_Depth = *self.Layer_Optical_Depth 
  ENDIF ELSE BEGIN
    Upwelling_Radiance  = INVALID
    Layer_Optical_Depth = INVALID
  ENDELSE

  
  ; Done
  CATCH, /CANCEL
 
END ; PRO RTS_Channel::Get_Property
