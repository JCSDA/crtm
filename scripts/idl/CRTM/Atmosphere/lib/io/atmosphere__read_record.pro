;+
; NAME:
;       Atmosphere::Read_Record
;
; PURPOSE:
;       The Atmosphere::Read_Record procedure method reads a Atmosphere record from file.
;
;       NOTE: This is considered a PRIVATE method not to be called outside
;             the context of reading an Atmosphere datafile.
;
; CALLING SEQUENCE:
;       Obj->[Atmosphere::]Read_Record, $
;         FileId       , $  ; Input
;         Quiet = Quiet     ; Input keyword
;         Debug = Debug     ; Input keyword
;
; INPUTS:
;       FileId:         The unit number for the already opened file from which
;                       the Atmosphere record is to be read.
;                       UNITS:      N/A
;                       TYPE:       INTEGER
;                       DIMENSION:  Scalar
;                       ATTRIBUTES: INTENT(IN)
;
; INPUT KEYWORDS:
;       Quiet:          Set this keyword to disable informational output
;                       If NOT SET => Record information is output. (DEFAULT)
;                          SET     => Record information is NOT output.
;                       UNITS:      N/A
;                       TYPE:       INTEGER
;                       DIMENSION:  Scalar
;                       ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Debug:          Set this keyword for debugging.
;                       If NOT SET => Error handler is enabled. (DEFAULT)
;                          SET     => Error handler is disabled; Routine
;                                     traceback output is enabled.
;                       UNITS:      N/A
;                       TYPE:       INTEGER
;                       DIMENSION:  Scalar
;                       ATTRIBUTES: INTENT(IN), OPTIONAL
;
; INCLUDE FILES:
;       atmosphere_parameters: Include file for Atmosphere specific parameters.
;
;       atmosphere_pro_err_handler: Include file for error handling.
;
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 29-Aug-2011
;                       paul.vandelst@noaa.gov
;
;-

PRO Atmosphere::Read_Record, $
  FileId       , $  ; Input
  Quiet = Quiet, $  ; Optional input
  Debug = Debug     ; Optional input
  

  ; Set up
  COMPILE_OPT HIDDEN
  @Atmosphere_parameters
  @Atmosphere_pro_err_handler
  ; ...Process keywords
  noisy = ~(KEYWORD_SET(Quiet))


  ; Read the data dimensions
  n_layers    = 0L
  n_absorbers = 0L
  n_clouds    = 0L
  n_aerosols  = 0L
  READU, FileID, n_layers, n_absorbers, n_clouds, n_aerosols


  ; Create the atmosphere structure
  self->Allocate, $
    n_layers     , $
    n_absorbers  , $
    Debug = Debug
  IF ( NOT self->Associated( Debug=Debug ) ) THEN $
    MESSAGE, 'Error creating output object.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Read the climatology model flag and absorber IDs
  climatology    = 0L
  absorber_id    = LONARR(n_absorbers)
  absorber_units = LONARR(n_absorbers)
  READU, FileID, climatology, absorber_id, absorber_units
  self->Set_Property, $
    Climatology     = climatology    , $
    Absorber_ID     = absorber_id    , $
    Absorber_Units  = absorber_units , $
    Debug           = debug


  ; Read the atmospheric profile data
  level_pressure  = DBLARR(n_layers+1L)
  pressure        = DBLARR(n_layers)
  temperature     = DBLARR(n_layers)
  absorber_amount = DBLARR(n_layers,n_absorbers)
  READU, FileID, &
    level_pressure , &    
    pressure       , & 
    temperature    , & 
    absorber_amount 
  self->Set_Property, $
    Level_Pressure  = level_pressure , $
    Pressure        = pressure       , $
    Temperature     = temperature    , $
    Absorber_Amount = absorber_amount, $
    Debug           = debug
    

  ; Read the cloud data
  



    0	    READ( fid,IOSTAT=io_stat ) atm%Level_Pressure, &
961	                               atm%Pressure, &
962	                               atm%Temperature, &
963	                               atm%Absorber
964	    IF ( io_stat /= 0 ) THEN
965	      WRITE( msg,'("Error reading atmospheric profile data. IOSTAT = ",i0)' ) io_stat
966	      CALL Read_Record_Cleanup(); RETURN
967	    END IF




  ; Read the Atmosphere data
  type               = 0L
  effective_radius   = DBLARR(n_layers)  
  effective_variance = DBLARR(n_layers)  
  water_content      = DBLARR(n_layers)  
  READU, FileId, type, $
                 effective_radius, $
                 effective_variance, $
                 water_content
  
  
  ; Load the Atmosphere data into the object
  self->Set_Property, Debug = Debug, $
    Type               = type              , $
    Effective_Radius   = effective_radius  , $
    Effective_Variance = effective_variance, $
    Water_Content      = water_content       
    
  
  ; Output info message
  IF ( Noisy ) THEN BEGIN
    MESSAGE, '  Atmosphere record n_Layers='+STRTRIM(n_layers,2)+$
             '; Type='+Atmosphere_TYPE_NAME[type], $
             /INFORMATIONAL
  ENDIF
  
END
;+
FUNCTION CRTM_Read_Atmosphere_Record, FileID     , $  ; Input
                                      Atm        , $  ; Output
                                      DEBUG=Debug     ; Optional input
;-
  ; Set up error handler
  ; --------------------
  @crtm_binary_io_error

  ; Read the data dimensions
  ; ------------------------
  n_Layers    = 0L
  n_Absorbers = 0L
  n_Clouds    = 0L
  n_Aerosols  = 0L
  READU, FileID, n_Layers   , $
                 n_Absorbers, $
                 n_Clouds   , $
                 n_Aerosols

  IF ( KEYWORD_SET(Debug) ) THEN BEGIN
    Msg = '  n_Layers='+STRTRIM(n_Layers,2)+$
          '; n_Absorbers='+STRTRIM(n_Absorbers,2)+$
          '; n_Clouds='+STRTRIM(n_Clouds,2)+$
          '; n_Aerosols='+STRTRIM(n_Aerosols,2)
    MESSAGE, Msg, /INFORMATIONAL
  ENDIF
  
  ; Allocate the atmosphere structure
  ; ---------------------------------
  result = CRTM_Allocate_Atmosphere( n_Layers   , $
                                     n_Absorbers, $
                                     n_Clouds   , $
                                     n_Aerosols , $
                                     Atm          )
  IF ( result NE SUCCESS ) THEN $
    MESSAGE, 'Error allocating Atmosphere structure', $
             /NONAME, /NOPRINT

  ; Read the climatology model flag and absorber IDs
  ; ------------------------------------------------
  Climatology = Atm.Climatology
  READU, FileID, Climatology, $
                 *Atm.Absorber_ID, $
                 *Atm.Absorber_Units
  Atm.Climatology = Climatology

  ; Read the atmospheric profile data
  ; ---------------------------------
  READU, FileID, *Atm.Level_Pressure, $
                 *Atm.Pressure      , $
                 *Atm.Temperature   , $
                 *Atm.Absorber      

  ; Read the cloud data
  ; -------------------
  IF ( n_Clouds GT 0 ) THEN BEGIN
    ; How many clouds?
    n_Input_Clouds = 0L
    READU, FileID, n_Input_Clouds
    IF ( n_Input_Clouds NE n_Clouds ) THEN $
      MESSAGE, 'Number of clouds, ' + $
               STRTRIM(n_Input_Clouds,2) + $
               ', is different from the size of Cloud structure array, ' + $
               STRTRIM(n_Clouds,2), $
               /NONAME, /NOPRINT
    ; Read cloud data
    result = CRTM_Read_Cloud_Record( FileID, *Atm.Cloud, DEBUG=Debug )
    IF ( result NE SUCCESS ) THEN $
      MESSAGE, 'Error reading Atmosphere Cloud elements', $
                 /NONAME, /NOPRINT
  ENDIF

  ; Read the Aerosol data
  ; ---------------------
  IF ( n_Aerosols GT 0 ) THEN BEGIN
    ; How many Aerosols?
    n_Input_Aerosols = 0L
    READU, FileID, n_Input_Aerosols
    IF ( n_Input_Aerosols NE n_Aerosols ) THEN $
      MESSAGE, 'Number of Aerosols, ' + $
               STRTRIM(n_Input_Aerosols,2) + $
               ', is different from the size of Aerosol structure array, ' + $
               STRTRIM(n_Aerosols,2), $
               /NONAME, /NOPRINT
    ; Read Aerosol data
    result = CRTM_Read_Aerosol_Record( FileID, *Atm.Aerosol, DEBUG=Debug )
    IF ( result NE SUCCESS ) THEN $
      MESSAGE, 'Error reading Atmosphere Aerosol element', $
                 /NONAME, /NOPRINT
  ENDIF

  ; Done
  ; ----
  CATCH, /CANCEL
  RETURN, SUCCESS
  
END
