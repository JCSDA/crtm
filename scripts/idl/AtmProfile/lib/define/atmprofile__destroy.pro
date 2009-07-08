;+
; NAME:
;       AtmProfile::Destroy
;
; PURPOSE:
;       The AtmProfile::Destroy procedure method deallocates and frees the
;       pointer components of an AtmProfile object.
;
;       NOTE: This method is called by the Cleanup procedure method, but
;             this Destroy method *can* be called outside the context of
;             object creation and destruction. That is, this is *not*
;             a lifecycle method.
;
; CALLING SEQUENCE:
;       Obj->[AtmProfile::]Destroy, $
;         No_Clear=No_Clear, $  ; Input keyword
;         Debug=Debug           ; Input keyword
;
; INPUT KEYWORD PARAMETERS:
;       No_Clear:    Set this keyword to NOT reinitialise the scalar
;                    components to their default values. The default
;                    is to also clear the scalar values.
;                    If NOT SET => scalar components are reinitialised (DEFAULT)
;                       SET,    => scalar components are NOT reinitialised
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
;
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
;                        parameter value definitions.
;
;       atmprofile_pro_err_handler: Error handler code for AtmProfile procedures.
;
; EXAMPLE:
;       After creating a AtmProfile object,
;
;         IDL> x = OBJ_NEW('AtmProfile')
;
;       and allocating data to it, the object internals can be reinitialised
;       like so,
;
;         IDL> x->Destroy
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 01-Jul-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO AtmProfile::Destroy, $
  No_Clear=No_Clear, $  ; Input keyword
  Debug=Debug           ; Input keyword
 
  ; Set up
  ; ...AtmProfile parameters
  @atmprofile_parameters
  ; ...Set up error handler
  @atmprofile_pro_err_handler
 
 
  ; Initialise the non-pointer members
  IF ( NOT KEYWORD_SET(No_Clear) ) THEN BEGIN
    self.Release           = ATMPROFILE_RELEASE
    self.Version           = ATMPROFILE_VERSION
    self.Profile           = 0L
    self.Description       = ''
    self.Climatology_Model = INVALID_CLIMATOLOGY
    self.Year              = IP_INVALID
    self.Month             = IP_INVALID
    self.Day               = IP_INVALID
    self.Hour              = IP_INVALID
    self.Latitude          = FP_INVALID
    self.Longitude         = FP_INVALID
    self.Surface_Altitude  = ZERO
  ENDIF


  ; If ALL pointer members are NOT associated, do nothing
  IF ( NOT self->Associated(Debug=Debug) ) THEN GOTO, Done


  ; Deallocate the pointer members and nullify
  PTR_FREE, $
    self.Absorber_ID        , $
    self.Absorber_Units_ID  , $
    self.Absorber_Units_Name, $
    self.Absorber_Units_LBL , $
    self.Level_Pressure     , $
    self.Level_Temperature  , $
    self.Level_Absorber     , $
    self.Level_Altitude     , $
    self.Layer_Pressure     , $
    self.Layer_Temperature  , $
    self.Layer_Absorber     , $
    self.Layer_Delta_Z      , $
    self.psysvar            , $
    self.xsysvar            , $
    self.ysysvar                
  self.Absorber_ID         = PTR_NEW()
  self.Absorber_Units_ID   = PTR_NEW()
  self.Absorber_Units_Name = PTR_NEW()
  self.Absorber_Units_LBL  = PTR_NEW()
  self.Level_Pressure      = PTR_NEW()
  self.Level_Temperature   = PTR_NEW()
  self.Level_Absorber      = PTR_NEW()
  self.Level_Altitude      = PTR_NEW()
  self.Layer_Pressure      = PTR_NEW()
  self.Layer_Temperature   = PTR_NEW()
  self.Layer_Absorber      = PTR_NEW()
  self.Layer_Delta_Z       = PTR_NEW()
  self.psysvar             = PTR_NEW()
  self.xsysvar             = PTR_NEW()
  self.ysysvar             = PTR_NEW()


  ; Reinitialise the dimensions
  self.n_Levels    = 0L
  self.n_Layers    = 0L
  self.n_Absorbers = 0L


  ; Decrement and test allocation counter
  self.n_Allocates = self.n_Allocates - 1
  IF ( self.n_Allocates NE 0 ) THEN $
    MESSAGE, 'Allocation counter /= 0, Value = ' + STRTRIM(self.n_Allocates, 2), $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch

  ; Done
  Done:
  CATCH, /CANCEL

END ; PRO AtmProfile::Destroy
