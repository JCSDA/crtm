;+
FUNCTION Inquire_TauProfile_netCDF, TauProfile_Filename, $  ; Input
                                    ; Dimensions
                                    N_LAYERS          = n_Layers         , $  ; Optional output
                                    N_CHANNELS        = n_Channels       , $  ; Optional output
                                    N_ANGLES          = n_Angles         , $  ; Optional output
                                    N_PROFILES        = n_Profiles       , $  ; Optional output
                                    N_MOLECULE_SETS   = n_Molecule_Sets  , $  ; Optional output
                                    ; Release/version info
                                    RELEASE           = Release          , $  ; Optional output
                                    VERSION           = Version          , $  ; Optional output
                                    ; Sensor IDs
                                    SENSOR_ID         = Sensor_ID        , $  ; Optional output
                                    WMO_SATELLITE_ID  = WMO_Satellite_ID , $  ; Optional output
                                    WMO_SENSOR_ID     = WMO_Sensor_ID    , $  ; Optional output
                                    ; Dimension descriptors
                                    LEVEL_PRESSURE    = Level_Pressure   , $  ; Optional output
                                    CHANNEL_LIST      = Channel_List     , $  ; Optional output
                                    ANGLE_LIST        = Angle_List       , $  ; Optional output
                                    PROFILE_LIST      = Profile_List     , $  ; Optional output
                                    MOLECULE_SET_LIST = Molecule_Set_List, $  ; Optional output
                                    ; Global attributes
                                    ID_TAG            = ID_Tag           , $  ; Optional output
                                    TITLE             = Title            , $  ; Optional output
                                    HISTORY           = History          , $  ; Optional output
                                    COMMENT           = Comment          , $  ; Optional output
                                    ; Miscellaneous
                                    DUMP              = Dump                  ; Optional input

;-

  ; Set up error handler
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN, FAILURE
  ENDIF    

  ; Include the TauProfile netCDF parameters
  @tauprofile_netcdf_parameters

  ; Check input
  n_Arguments = 1
  IF ( N_PARAMS() LT n_Arguments ) THEN $
    MESSAGE, 'Invalid number of arguments.', $
             /NONAME, /NOPRINT

  IF ( NOT Valid_String( TauProfile_Filename ) ) THEN $
    MESSAGE, 'Input TauProfile_Filename argument not defined!', $
             /NONAME, /NOPRINT

  ; Construct the list of data to read from file
  variableList = [ LEVEL_PRESSURE_VARNAME   , $
                   CHANNEL_LIST_VARNAME     , $
                   ANGLE_LIST_VARNAME       , $
                   PROFILE_LIST_VARNAME     , $
                   MOLECULE_SET_LIST_VARNAME  ]

  ; Read the data file
  result = Read_netCDF( TauProfile_Filename       , $
                        TauProfile                , $
                        VARIABLE_LIST=variableList, $
                        /GLOBAL_ATTRIBUTES        , $
                        /QUIET                      )
  
  ; Stuff the data into the return variables   
  ; Dimensions
  n_Layers        = TauProfile.n_Layers       
  n_Channels      = TauProfile.n_Channels     
  n_Angles        = TauProfile.n_Angles       
  n_Profiles      = TauProfile.n_Profiles     
  n_Molecule_Sets = TauProfile.n_Molecule_Sets
  ; Dimension descriptors
  Level_Pressure    = TauProfile.Level_Pressure   
  Channel_List      = TauProfile.Channel_List     
  Angle_List        = TauProfile.Angle_List       
  Profile_List      = TauProfile.Profile_List     
  Molecule_Set_List = TauProfile.Molecule_Set_List
  ; Global attributes
  Release          = TauProfile.Release
  Version          = TauProfile.Version
  Sensor_ID        = TauProfile.Sensor_ID  
  WMO_Satellite_ID = TauProfile.WMO_Satellite_ID
  WMO_Sensor_ID    = TauProfile.WMO_Sensor_ID
  ID_Tag           = TauProfile.ID_Tag       
  Title            = TauProfile.Title        
  History          = TauProfile.History      
  Comment          = TauProfile.Comment      

  ; Screen dump if required
  IF ( KEYWORD_SET( Dump ) ) THEN BEGIN
    PRINT, FORMAT = '(/2x, a )', TauProfile_Filename
    PRINT, FORMAT = '( 5x, "Number of layers:        ", i4 )', n_Layers
    PRINT, FORMAT = '( 5x, "Number of channels:      ", i4 )', n_Channels
    PRINT, FORMAT = '( 5x, "Number of angles:        ", i4 )', n_Angles
    PRINT, FORMAT = '( 5x, "Number of profiles:      ", i4 )', n_Profiles
    PRINT, FORMAT = '( 5x, "Number of molecule sets: ", i4 )', n_Molecule_Sets
    PRINT, FORMAT = '(/5x, a, ": ",i4)', RELEASE_GATTNAME, Release
    PRINT, FORMAT = '( 5x, a, ": ",i4)', VERSION_GATTNAME, Version      
    PRINT, FORMAT = '( 5x, a, ": ", a)', SENSOR_ID_GATTNAME       , Sensor_ID       
    PRINT, FORMAT = '( 5x, a, ": ",i4)', WMO_SATELLITE_ID_GATTNAME, WMO_Satellite_ID      
    PRINT, FORMAT = '( 5x, a, ": ",i4)', WMO_SENSOR_ID_GATTNAME   , WMO_Sensor_ID         
    PRINT, FORMAT = '( 5x, a, ": ", a)', TITLE_GATTNAME  , Title
    PRINT, FORMAT = '( 5x, a, ": ", a)', HISTORY_GATTNAME, History      
    PRINT, FORMAT = '( 5x, a, ": ", a)', COMMENT_GATTNAME, Comment      
    PRINT, FORMAT = '( 5x, a, ": ", a)', ID_TAG_GATTNAME , ID_Tag       
    PRINT, FORMAT = '(/5x, a, ":")', LEVEL_PRESSURE_VARNAME
    PRINT, FORMAT = '( 8( 2x, f8.3 ) )', Level_Pressure
    PRINT, FORMAT = '(/5x, a, ":")', CHANNEL_LIST_VARNAME
    PRINT, FORMAT = '( 12( 2x, i4 ) )', Channel_List
    PRINT, FORMAT = '(/5x, a, ":")', ANGLE_LIST_VARNAME
    PRINT, FORMAT = '( 12( 2x, f4.2 ) )', Angle_List
    PRINT, FORMAT = '(/5x, a, ":")', PROFILE_LIST_VARNAME
    PRINT, FORMAT = '( 12( 2x, i4 ) )', Profile_List
    PRINT, FORMAT = '(/5x, a, ":")', MOLECULE_SET_LIST_VARNAME
    PRINT, FORMAT = '( 12( 2x, i4 ) )', Molecule_Set_List
  ENDIF

  ; Done
  RETURN, SUCCESS

END ; FUNCTION Inquire_TauProfile_netCDF
