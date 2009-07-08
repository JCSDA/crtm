;+
; NAME:
;       AtmProfile::Allocate
;
; PURPOSE:
;       The AtmProfile::Allocate procedure method allocates the SRF
;       object data arrays.
;
; CALLING SEQUENCE:
;       Obj->[AtmProfile::]Allocate, $
;         n_Layers   , $  ; Input
;         n_Absorbers, $  ; Input
;         Debug=Debug     ; Input keyword
;
; INPUTS:
;       n_Layers:    The number of atmospheric layers.
;                    Must be > 0.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN)
;                    
;       n_Absorbers: The number of atmospheric absorbing species.
;                    Must be > 0.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN)
;                    
; INPUT KEYWORD PARAMETERS:
;       Debug:       Set this keyword for debugging. If set then:
;                    - the error handler for this function is disabled
;                      so that execution halts where the error occurs,
;                    - more verbose output is produced.
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
;       After creating a AtmProfile object,
;
;         IDL> x = OBJ_NEW('AtmProfile')
;
;       it can be allocated to the required dimensions like so:
;
;         IDL> n_Layers = 100
;         IDL> n_Absorbers = 2
;         IDL> x->Allocate, n_Layers, n_Absorbers
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 02-Jul-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO AtmProfile::Allocate, $
  n_Layers   , $  ; Input
  n_Absorbers, $  ; Input
  Debug=Debug     ; Input keyword

  ; Set up
  ; ...Parameters
  @atmprofile_parameters
  ; ...Set up error handler
  @atmprofile_pro_err_handler
  ; ...Check dimension input
  IF ( N_ELEMENTS(n_Layers   ) EQ 0 OR $
       N_ELEMENTS(n_Absorbers) EQ 0    ) THEN $
    MESSAGE, 'Input AtmProfile dimensions are not defined.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  IF ( n_Layers    LT 1 OR $
       n_Absorbers LT 1    ) THEN $
    MESSAGE, 'Input AtmProfile dimensions must all be > 0.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  n_Levels = n_Layers + 1
  IF ( n_Absorbers GT ATMPROFILE_MAX_N_ABSORBERS ) THEN $
    MESSAGE, 'Input AtmProfile absorber dimension exceeds maximum of ' + $
             STRTRIM(ATMPROFILE_MAX_N_ABSORBERS,2), $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ; ...Check if ANY pointers are already associated
  ; ...If they are, deallocate them but leave scalars.
  IF ( self->Associated(/ANY_Test, Debug=Debug) ) THEN $
    self->Destroy, /No_Clear, Debug=Debug


  ; Perform the allocations 
  self.Absorber_ID         = PTR_NEW(LONARR(n_Absorbers))
  self.Absorber_Units_ID   = PTR_NEW(LONARR(n_Absorbers))
  self.Absorber_Units_Name = PTR_NEW(STRARR(n_Absorbers))
  self.Absorber_Units_LBL  = PTR_NEW(STRARR(n_Absorbers))

  self.Level_Pressure    = PTR_NEW(DBLARR(n_Levels))
  self.Level_Temperature = PTR_NEW(DBLARR(n_Levels))
  self.Level_Absorber    = PTR_NEW(DBLARR(n_Levels, n_Absorbers))
  self.Level_Altitude    = PTR_NEW(DBLARR(n_Levels))

  self.Layer_Pressure    = PTR_NEW(DBLARR(n_Layers))
  self.Layer_Temperature = PTR_NEW(DBLARR(n_Layers))
  self.Layer_Absorber    = PTR_NEW(DBLARR(n_Layers, n_Absorbers))
  self.Layer_Delta_Z     = PTR_NEW(DBLARR(n_Layers))

  self.xsysvar = PTR_NEW(REPLICATE(!X,n_Absorbers+1))
  self.ysysvar = PTR_NEW(REPLICATE(!Y,n_Absorbers+1))
  self.psysvar = PTR_NEW(REPLICATE(!P,n_Absorbers+1))

  ; Assign the dimensions
  self.n_Levels    = n_Levels
  self.n_Layers    = n_Layers
  self.n_Absorbers = n_Absorbers


  ; Increment and test allocation counter
  self.n_Allocates = self.n_Allocates + 1
  IF ( self.n_Allocates NE 1 ) THEN $
    MESSAGE, 'Allocation counter /= 1, Value = ' + STRTRIM(self.n_Allocates,2), $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
 
  ; Done
  CATCH, /CANCEL
 
END ; PRO AtmProfile::Allocate
