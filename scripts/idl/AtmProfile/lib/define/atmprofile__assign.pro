;+
; NAME:
;       AtmProfile::Assign
;
; PURPOSE:
;       The AtmProfile::Assign procedure method copies a valid AtmProfile object.
;
; CALLING SEQUENCE:
;       Obj->[AtmProfile::]Assign, $
;         new        , $  ; Output
;         Debug=Debug     ; Input keyword
;
; OUTPUT:
;       new:         A deep copy of the AtmProfile object.
;                    UNITS:      N/A
;                    TYPE:       AtmProfile object
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN)
;                    
; INPUT KEYWORD PARAMETERS:
;       Debug:       Set this keyword for debugging.
;                    If NOT SET => Error handler is enabled. (DEFAULT)
;                       SET     => Error handler is disabled; Routine
;                                  traceback output is enabled.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
;
; INCLUDE FILES:
;       atmprofile_parameters: Include file containing AtmProfile specific
;                              parameter value definitions.
;
;       atmprofile_pro_err_handler: Error handler code for AtmProfile procedures.
;
; EXAMPLE:
;       Given an instance of a AtmProfile object,
;
;         IDL> HELP, x
;         X               OBJREF    = <ObjHeapVar8(AtmProfile)>
;
;       a new instance of the data object is created by:
;
;         IDL> x->Assign,y
;         IDL> help, y
;         Y               OBJREF    = <ObjHeapVar12(AtmProfile)>
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 01-Jul-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO AtmProfile::Assign, $
  new, $       ; Output
  Debug=Debug  ; Input keyword

  ; Set up
  ; ...AtmProfile parameters
  @atmprofile_parameters
  ; ...Set up error handler
  @atmprofile_pro_err_handler
  ; ...ALL *input* pointers must be associated
  IF ( self->Associated(Debug=Debug) EQ FALSE ) THEN $
    MESSAGE, 'Some or all input AtmProfile pointer members are NOT associated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Destroy output object if defined.
  IF ( N_ELEMENTS(new) GT 0 ) THEN $
    IF ( SIZE(new, /TNAME) EQ 'OBJREF' ) THEN OBJ_DESTROY, new, Debug=Debug


  ; Create a new object reference
  new = OBJ_NEW('AtmProfile',Debug=Debug)


  ; Allocate the output object
  new->Allocate, self.n_Layers, self.n_Absorbers, Debug=Debug


  ; Assign data components
  ; ...scalar values
  new.Release           = self.Release          
  new.Version           = self.Version          
  new.PD_StrLen         = self.PD_StrLen        
  new.AUN_StrLen        = self.AUN_StrLen       
  new.n_Levels          = self.n_Levels         
  new.n_Layers          = self.n_Layers         
  new.n_Absorbers       = self.n_Absorbers      
  new.Description       = self.Description      
  new.Climatology_Model = self.Climatology_Model
  new.Year              = self.Year             
  new.Month             = self.Month            
  new.Day               = self.Day              
  new.Hour              = self.Hour             
  new.Latitude          = self.Latitude         
  new.Longitude         = self.Longitude        
  new.Surface_Altitude  = self.Surface_Altitude 
  ; ...pointer values
  *new.Absorber_ID           = *self.Absorber_ID          
  *new.Absorber_Units_ID     = *self.Absorber_Units_ID    
  *new.Absorber_Units_Name   = *self.Absorber_Units_Name  
  *new.Absorber_Units_LBLRTM = *self.Absorber_Units_LBLRTM
  *new.Level_Pressure        = *self.Level_Pressure       
  *new.Level_Temperature     = *self.Level_Temperature    
  *new.Level_Absorber        = *self.Level_Absorber       
  *new.Level_Altitude        = *self.Level_Altitude       
  *new.Layer_Pressure        = *self.Layer_Pressure       
  *new.Layer_Temperature     = *self.Layer_Temperature    
  *new.Layer_Absorber        = *self.Layer_Absorber       
  *new.Layer_Delta_Z         = *self.Layer_Delta_Z        
  *new.xsysvar = *self.xsysvar
  *new.ysysvar = *self.ysysvar
  *new.psysvar = *self.psysvar


  ; Done
  CATCH, /CANCEL

END ; PRO AtmProfile::Assign
