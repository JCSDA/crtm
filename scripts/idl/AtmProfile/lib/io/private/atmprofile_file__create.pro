;
; NAME:
;       AtmProfile_File::Create
;
; PURPOSE:
;       The AtmProfile_File::Create procedure method creates an AtmProfile_File
;       for writing.
;
;       NOTE: This method should be considered PRIVATE to the class
;             and should not be invoked outside AtmProfile_File methods.
;
;
; CALLING SEQUENCE:
;       Obj->[AtmProfile_File::]Create, $
;         Debug = Debug  ;  Input keyword
;
; INPUT KEYWORDS:
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
;       atmprofile_file parameters: Include file containing AtmProfile_File specific
;                                   parameter value definitions.
;
;       atmprofile_pro_err_handler: Error handler code for AtmProfile procedures.
;
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 06-Jul-2009
;                       paul.vandelst@noaa.gov
;-

PRO AtmProfile_File::Create, $
  Debug = Debug
  
  ; Set up
  COMPILE_OPT HIDDEN
  ; ...netCDF parameters
  @atmprofile_file_parameters
  ; ...Set up error handler
  @atmprofile_pro_err_handler
  

  ; Create the data file
  fid = NCDF_CREATE(self.filename, /CLOBBER)
  NCDF_CONTROL, fid, /VERBOSE

  
  ; Define the known dimensions
  ; ...First check they have meaningful values
  IF ( self.n_Layers    LT 1 OR $
       self.n_Absorbers LT 1 OR $
       self.n_Levels    NE self.n_Layers+1 ) THEN $
    MESSAGE, 'AtmProfile_File dimensions are invalid!', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ; ...Define them 
  n_Levels_DimId    = NCDF_DIMDEF(fid, LEVEL_DIMNAME         , self.n_Levels)
  n_Layers_DimId    = NCDF_DIMDEF(fid, LAYER_DIMNAME         , self.n_Layers)
  n_Absorbers_DimId = NCDF_DIMDEF(fid, ABSORBER_DIMNAME      , self.n_Absorbers)
  pdsl_DimId        = NCDF_DIMDEF(fid, DESCRIPTION_DIMNAME   , PDSL)
  aunsl_DimId       = NCDF_DIMDEF(fid, ABSORBER_UNITS_DIMNAME, AUNSL)
  n_Profiles_DimId  = NCDF_DIMDEF(fid, PROFILE_DIMNAME       , /UNLIMITED)


  ; Define the variables and attributes
  ; ...The Description
  VarId = NCDF_VARDEF( fid, DESCRIPTION_VARNAME, [pdsl_DimId,n_Profiles_DimId], /CHAR)
  NCDF_ATTPUT, fid, VarId, LONGNAME_ATTNAME   , DESCRIPTION_LONGNAME   , /CHAR
  NCDF_ATTPUT, fid, VarId, DESCRIPTION_ATTNAME, DESCRIPTION_DESCRIPTION, /CHAR
  NCDF_ATTPUT, fid, VarId, UNITS_ATTNAME      , DESCRIPTION_UNITS      , /CHAR
  NCDF_ATTPUT, fid, VarId, FILLVALUE_ATTNAME  , DESCRIPTION_FILLVALUE  , /CHAR
  
  ; ...The Climatology_Model
  VarId = NCDF_VARDEF( fid, CLIMATOLOGY_MODEL_VARNAME, n_Profiles_DimId , /LONG)
  NCDF_ATTPUT, fid, VarId, LONGNAME_ATTNAME   , CLIMATOLOGY_MODEL_LONGNAME   , /CHAR
  NCDF_ATTPUT, fid, VarId, DESCRIPTION_ATTNAME, CLIMATOLOGY_MODEL_DESCRIPTION, /CHAR
  NCDF_ATTPUT, fid, VarId, UNITS_ATTNAME      , CLIMATOLOGY_MODEL_UNITS      , /CHAR
  NCDF_ATTPUT, fid, VarId, FILLVALUE_ATTNAME  , CLIMATOLOGY_MODEL_FILLVALUE  , /CHAR
  
  ; ...The Year
  VarId = NCDF_VARDEF( fid, YEAR_VARNAME, n_Profiles_DimId, /LONG)
  NCDF_ATTPUT, fid, VarId, LONGNAME_ATTNAME   , YEAR_LONGNAME   , /CHAR
  NCDF_ATTPUT, fid, VarId, DESCRIPTION_ATTNAME, YEAR_DESCRIPTION, /CHAR
  NCDF_ATTPUT, fid, VarId, UNITS_ATTNAME      , YEAR_UNITS      , /CHAR
  NCDF_ATTPUT, fid, VarId, FILLVALUE_ATTNAME  , YEAR_FILLVALUE  , /CHAR
  
  ; ...The Month
  VarId = NCDF_VARDEF( fid, MONTH_VARNAME, n_Profiles_DimId, /LONG)
  NCDF_ATTPUT, fid, VarId, LONGNAME_ATTNAME   , MONTH_LONGNAME   , /CHAR
  NCDF_ATTPUT, fid, VarId, DESCRIPTION_ATTNAME, MONTH_DESCRIPTION, /CHAR
  NCDF_ATTPUT, fid, VarId, UNITS_ATTNAME      , MONTH_UNITS      , /CHAR
  NCDF_ATTPUT, fid, VarId, FILLVALUE_ATTNAME  , MONTH_FILLVALUE  , /CHAR
  
  ; ...The Day
  VarId = NCDF_VARDEF( fid, DAY_VARNAME, n_Profiles_DimId, /LONG)
  NCDF_ATTPUT, fid, VarId, LONGNAME_ATTNAME   , DAY_LONGNAME   , /CHAR
  NCDF_ATTPUT, fid, VarId, DESCRIPTION_ATTNAME, DAY_DESCRIPTION, /CHAR
  NCDF_ATTPUT, fid, VarId, UNITS_ATTNAME      , DAY_UNITS      , /CHAR
  NCDF_ATTPUT, fid, VarId, FILLVALUE_ATTNAME  , DAY_FILLVALUE  , /CHAR
  
  ; ...The Hour
  VarId = NCDF_VARDEF( fid, HOUR_VARNAME, n_Profiles_DimId, /LONG)
  NCDF_ATTPUT, fid, VarId, LONGNAME_ATTNAME   , HOUR_LONGNAME   , /CHAR
  NCDF_ATTPUT, fid, VarId, DESCRIPTION_ATTNAME, HOUR_DESCRIPTION, /CHAR
  NCDF_ATTPUT, fid, VarId, UNITS_ATTNAME      , HOUR_UNITS      , /CHAR
  NCDF_ATTPUT, fid, VarId, FILLVALUE_ATTNAME  , HOUR_FILLVALUE  , /CHAR
  
  ; ...The Latitude
  VarId = NCDF_VARDEF( fid, LATITUDE_VARNAME, n_Profiles_DimId, /DOUBLE)
  NCDF_ATTPUT, fid, VarId, LONGNAME_ATTNAME   , LATITUDE_LONGNAME   , /CHAR
  NCDF_ATTPUT, fid, VarId, DESCRIPTION_ATTNAME, LATITUDE_DESCRIPTION, /CHAR
  NCDF_ATTPUT, fid, VarId, UNITS_ATTNAME      , LATITUDE_UNITS      , /CHAR
  NCDF_ATTPUT, fid, VarId, FILLVALUE_ATTNAME  , LATITUDE_FILLVALUE  , /CHAR
  
  ; ...The Longitude
  VarId = NCDF_VARDEF( fid, LONGITUDE_VARNAME, n_Profiles_DimId, /DOUBLE)
  NCDF_ATTPUT, fid, VarId, LONGNAME_ATTNAME   , LONGITUDE_LONGNAME   , /CHAR
  NCDF_ATTPUT, fid, VarId, DESCRIPTION_ATTNAME, LONGITUDE_DESCRIPTION, /CHAR
  NCDF_ATTPUT, fid, VarId, UNITS_ATTNAME      , LONGITUDE_UNITS      , /CHAR
  NCDF_ATTPUT, fid, VarId, FILLVALUE_ATTNAME  , LONGITUDE_FILLVALUE  , /CHAR
  
  ; ...The Surface_Altitude
  VarId = NCDF_VARDEF( fid, SURFACE_ALTITUDE_VARNAME, n_Profiles_DimId, /DOUBLE)
  NCDF_ATTPUT, fid, VarId, LONGNAME_ATTNAME   , SURFACE_ALTITUDE_LONGNAME   , /CHAR
  NCDF_ATTPUT, fid, VarId, DESCRIPTION_ATTNAME, SURFACE_ALTITUDE_DESCRIPTION, /CHAR
  NCDF_ATTPUT, fid, VarId, UNITS_ATTNAME      , SURFACE_ALTITUDE_UNITS      , /CHAR
  NCDF_ATTPUT, fid, VarId, FILLVALUE_ATTNAME  , SURFACE_ALTITUDE_FILLVALUE  , /CHAR
  
  ; ...The Absorber_Id
  VarId = NCDF_VARDEF( fid, ABSORBER_ID_VARNAME, n_Absorbers_DimId, /LONG)
  NCDF_ATTPUT, fid, VarId, LONGNAME_ATTNAME   , ABSORBER_ID_LONGNAME   , /CHAR
  NCDF_ATTPUT, fid, VarId, DESCRIPTION_ATTNAME, ABSORBER_ID_DESCRIPTION, /CHAR
  NCDF_ATTPUT, fid, VarId, UNITS_ATTNAME      , ABSORBER_ID_UNITS      , /CHAR
  NCDF_ATTPUT, fid, VarId, FILLVALUE_ATTNAME  , ABSORBER_ID_FILLVALUE  , /CHAR

  ; ...The Absorber_Units_Id
  VarId = NCDF_VARDEF( fid, ABSORBER_UNITS_ID_VARNAME, n_Absorbers_DimId, /LONG)
  NCDF_ATTPUT, fid, VarId, LONGNAME_ATTNAME   , ABSORBER_UNITS_ID_LONGNAME   , /CHAR
  NCDF_ATTPUT, fid, VarId, DESCRIPTION_ATTNAME, ABSORBER_UNITS_ID_DESCRIPTION, /CHAR
  NCDF_ATTPUT, fid, VarId, UNITS_ATTNAME      , ABSORBER_UNITS_ID_UNITS      , /CHAR
  NCDF_ATTPUT, fid, VarId, FILLVALUE_ATTNAME  , ABSORBER_UNITS_ID_FILLVALUE  , /CHAR

  ; ...The Absorber_Units_Name
  VarId = NCDF_VARDEF( fid, ABSORBER_UNITS_NAME_VARNAME, [aunsl_DimId,n_Absorbers_DimId], /CHAR)
  NCDF_ATTPUT, fid, VarId, LONGNAME_ATTNAME   , ABSORBER_UNITS_NAME_LONGNAME   , /CHAR
  NCDF_ATTPUT, fid, VarId, DESCRIPTION_ATTNAME, ABSORBER_UNITS_NAME_DESCRIPTION, /CHAR
  NCDF_ATTPUT, fid, VarId, UNITS_ATTNAME      , ABSORBER_UNITS_NAME_UNITS      , /CHAR
  NCDF_ATTPUT, fid, VarId, FILLVALUE_ATTNAME  , ABSORBER_UNITS_NAME_FILLVALUE  , /CHAR
  
  ; ...The Level_Pressure
  VarId = NCDF_VARDEF( fid, LEVEL_PRESSURE_VARNAME, [n_Levels_DimId,n_Profiles_DimId], /DOUBLE)
  NCDF_ATTPUT, fid, VarId, LONGNAME_ATTNAME   , LEVEL_PRESSURE_LONGNAME   , /CHAR
  NCDF_ATTPUT, fid, VarId, DESCRIPTION_ATTNAME, LEVEL_PRESSURE_DESCRIPTION, /CHAR
  NCDF_ATTPUT, fid, VarId, UNITS_ATTNAME      , LEVEL_PRESSURE_UNITS      , /CHAR
  NCDF_ATTPUT, fid, VarId, FILLVALUE_ATTNAME  , LEVEL_PRESSURE_FILLVALUE  , /CHAR
  
  ; ...The Level_Temperature
  VarId = NCDF_VARDEF( fid, LEVEL_TEMPERATURE_VARNAME, [n_Levels_DimId,n_Profiles_DimId], /DOUBLE)
  NCDF_ATTPUT, fid, VarId, LONGNAME_ATTNAME   , LEVEL_TEMPERATURE_LONGNAME   , /CHAR
  NCDF_ATTPUT, fid, VarId, DESCRIPTION_ATTNAME, LEVEL_TEMPERATURE_DESCRIPTION, /CHAR
  NCDF_ATTPUT, fid, VarId, UNITS_ATTNAME      , LEVEL_TEMPERATURE_UNITS      , /CHAR
  NCDF_ATTPUT, fid, VarId, FILLVALUE_ATTNAME  , LEVEL_TEMPERATURE_FILLVALUE  , /CHAR
  
  ; ...The Level_Absorber
  VarId = NCDF_VARDEF( fid, LEVEL_ABSORBER_VARNAME, [n_Levels_DimId,n_Absorbers_DimId,n_Profiles_DimId], /DOUBLE)
  NCDF_ATTPUT, fid, VarId, LONGNAME_ATTNAME   , LEVEL_ABSORBER_LONGNAME   , /CHAR
  NCDF_ATTPUT, fid, VarId, DESCRIPTION_ATTNAME, LEVEL_ABSORBER_DESCRIPTION, /CHAR
  NCDF_ATTPUT, fid, VarId, UNITS_ATTNAME      , LEVEL_ABSORBER_UNITS      , /CHAR
  NCDF_ATTPUT, fid, VarId, FILLVALUE_ATTNAME  , LEVEL_ABSORBER_FILLVALUE  , /CHAR
  
  ; ...The Level_Altitude
  VarId = NCDF_VARDEF( fid, LEVEL_ALTITUDE_VARNAME, [n_Levels_DimId,n_Profiles_DimId], /DOUBLE)
  NCDF_ATTPUT, fid, VarId, LONGNAME_ATTNAME   , LEVEL_ALTITUDE_LONGNAME   , /CHAR
  NCDF_ATTPUT, fid, VarId, DESCRIPTION_ATTNAME, LEVEL_ALTITUDE_DESCRIPTION, /CHAR
  NCDF_ATTPUT, fid, VarId, UNITS_ATTNAME      , LEVEL_ALTITUDE_UNITS      , /CHAR
  NCDF_ATTPUT, fid, VarId, FILLVALUE_ATTNAME  , LEVEL_ALTITUDE_FILLVALUE  , /CHAR
  
  ; ...The Layer_Pressure
  VarId = NCDF_VARDEF( fid, LAYER_PRESSURE_VARNAME, [n_Layers_DimId,n_Profiles_DimId], /DOUBLE)
  NCDF_ATTPUT, fid, VarId, LONGNAME_ATTNAME   , LAYER_PRESSURE_LONGNAME   , /CHAR
  NCDF_ATTPUT, fid, VarId, DESCRIPTION_ATTNAME, LAYER_PRESSURE_DESCRIPTION, /CHAR
  NCDF_ATTPUT, fid, VarId, UNITS_ATTNAME      , LAYER_PRESSURE_UNITS      , /CHAR
  NCDF_ATTPUT, fid, VarId, FILLVALUE_ATTNAME  , LAYER_PRESSURE_FILLVALUE  , /CHAR
  
  ; ...The Layer_Temperature
  VarId = NCDF_VARDEF( fid, LAYER_TEMPERATURE_VARNAME, [n_Layers_DimId,n_Profiles_DimId], /DOUBLE)
  NCDF_ATTPUT, fid, VarId, LONGNAME_ATTNAME   , LAYER_TEMPERATURE_LONGNAME   , /CHAR
  NCDF_ATTPUT, fid, VarId, DESCRIPTION_ATTNAME, LAYER_TEMPERATURE_DESCRIPTION, /CHAR
  NCDF_ATTPUT, fid, VarId, UNITS_ATTNAME      , LAYER_TEMPERATURE_UNITS      , /CHAR
  NCDF_ATTPUT, fid, VarId, FILLVALUE_ATTNAME  , LAYER_TEMPERATURE_FILLVALUE  , /CHAR
  
  ; ...The Layer_Absorber
  VarId = NCDF_VARDEF( fid, LAYER_ABSORBER_VARNAME, [n_Layers_DimId,n_Absorbers_DimId,n_Profiles_DimId], /DOUBLE)
  NCDF_ATTPUT, fid, VarId, LONGNAME_ATTNAME   , LAYER_ABSORBER_LONGNAME   , /CHAR
  NCDF_ATTPUT, fid, VarId, DESCRIPTION_ATTNAME, LAYER_ABSORBER_DESCRIPTION, /CHAR
  NCDF_ATTPUT, fid, VarId, UNITS_ATTNAME      , LAYER_ABSORBER_UNITS      , /CHAR
  NCDF_ATTPUT, fid, VarId, FILLVALUE_ATTNAME  , LAYER_ABSORBER_FILLVALUE  , /CHAR
  
  ; ...The Layer_Delta_Z
  VarId = NCDF_VARDEF( fid, LAYER_DELTA_Z_VARNAME, [n_Layers_DimId,n_Profiles_DimId], /DOUBLE)
  NCDF_ATTPUT, fid, VarId, LONGNAME_ATTNAME   , LAYER_DELTA_Z_LONGNAME   , /CHAR
  NCDF_ATTPUT, fid, VarId, DESCRIPTION_ATTNAME, LAYER_DELTA_Z_DESCRIPTION, /CHAR
  NCDF_ATTPUT, fid, VarId, UNITS_ATTNAME      , LAYER_DELTA_Z_UNITS      , /CHAR
  NCDF_ATTPUT, fid, VarId, FILLVALUE_ATTNAME  , LAYER_DELTA_Z_FILLVALUE  , /CHAR


  ; Done
  NCDF_CLOSE, fid
  CATCH, /CANCEL

END ; PRO AtmProfile_File::Create

