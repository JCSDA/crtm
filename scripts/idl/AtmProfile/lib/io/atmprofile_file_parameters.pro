;+
; Parameters for AtmProfile file I/O routines

  @atmprofile_parameters
  
  ; Global attribute names. Case sensitive
  TITLE_GATTNAME          = 'title'
  HISTORY_GATTNAME        = 'history'
  COMMENT_GATTNAME        = 'comment'
  RELEASE_GATTNAME        = 'Release'
  VERSION_GATTNAME        = 'Version'
  PROFILE_SET_ID_GATTNAME = 'Profile_Set_Id'

  ; Dimension names
  LEVEL_DIMNAME          = 'n_levels'
  LAYER_DIMNAME          = 'n_layers'
  ABSORBER_DIMNAME       = 'n_absorbers'
  PROFILE_DIMNAME        = 'n_profiles'
  DESCRIPTION_DIMNAME    = 'pdsl'
  ABSORBER_UNITS_DIMNAME = 'aunsl'
  ; ...Default dimension values
  PDSL  = 512L ; Profile description string length
  AUNSL = 32L  ; Absorber units name string length
  
  ; Variable names
  DESCRIPTION_VARNAME         = 'profile_description'
  CLIMATOLOGY_MODEL_VARNAME   = 'climatology_model'
  YEAR_VARNAME                = 'year'
  MONTH_VARNAME               = 'month'
  DAY_VARNAME                 = 'day'
  HOUR_VARNAME                = 'hour'
  LATITUDE_VARNAME            = 'latitude'
  LONGITUDE_VARNAME           = 'longitude'
  SURFACE_ALTITUDE_VARNAME    = 'surface_altitude'
  ABSORBER_ID_VARNAME         = 'absorber_id'
  ABSORBER_UNITS_ID_VARNAME   = 'absorber_units_id'
  ABSORBER_UNITS_NAME_VARNAME = 'absorber_units_name'
  LEVEL_PRESSURE_VARNAME      = 'level_pressure'
  LEVEL_TEMPERATURE_VARNAME   = 'level_temperature'
  LEVEL_ABSORBER_VARNAME      = 'level_absorber'
  LEVEL_ALTITUDE_VARNAME      = 'level_altitude'
  LAYER_PRESSURE_VARNAME      = 'layer_pressure'
  LAYER_TEMPERATURE_VARNAME   = 'layer_temperature'
  LAYER_ABSORBER_VARNAME      = 'layer_absorber'
  LAYER_DELTA_Z_VARNAME       = 'layer_delta_z'

  ; Variable long name attribute.
  LONGNAME_ATTNAME = 'long_name'

  DESCRIPTION_LONGNAME         = 'Profile Description'
  CLIMATOLOGY_MODEL_LONGNAME   = 'Climatology Model'
  YEAR_LONGNAME                = 'Year'
  MONTH_LONGNAME               = 'Month'
  DAY_LONGNAME                 = 'Day'
  HOUR_LONGNAME                = 'Hour'
  LATITUDE_LONGNAME            = 'Latitude'
  LONGITUDE_LONGNAME           = 'Longitude'
  SURFACE_ALTITUDE_LONGNAME    = 'Surface Altitude'
  ABSORBER_ID_LONGNAME         = 'Absorber ID'
  ABSORBER_UNITS_ID_LONGNAME   = 'Absorber Units ID'
  ABSORBER_UNITS_NAME_LONGNAME = 'Absorber Units Name'
  LEVEL_PRESSURE_LONGNAME      = 'Level pressure'
  LEVEL_TEMPERATURE_LONGNAME   = 'Level temperature'
  LEVEL_ABSORBER_LONGNAME      = 'Level absorber'
  LEVEL_ALTITUDE_LONGNAME      = 'Level altitude'
  LAYER_PRESSURE_LONGNAME      = 'Layer pressure'
  LAYER_TEMPERATURE_LONGNAME   = 'Layer temperature'
  LAYER_ABSORBER_LONGNAME      = 'Layer absorber'
  LAYER_DELTA_Z_LONGNAME       = 'Layer thickness'

  ; Variable description attribute.
  DESCRIPTION_ATTNAME = 'description'

  DESCRIPTION_DESCRIPTION         = 'Description of atmospheric profile and modification'
  CLIMATOLOGY_MODEL_DESCRIPTION   = 'Climatology model associated with profile date/time/location.'
  YEAR_DESCRIPTION                = 'Year associated with profile data'
  MONTH_DESCRIPTION               = 'Month associated with profile data'
  DAY_DESCRIPTION                 = 'Day associated with profile data'
  HOUR_DESCRIPTION                = 'Hour associated with profile data'
  LATITUDE_DESCRIPTION            = 'Latitude of profile location'
  LONGITUDE_DESCRIPTION           = 'Longitude of profile location'
  SURFACE_ALTITUDE_DESCRIPTION    = 'Surface altitude of profile'
  ABSORBER_ID_DESCRIPTION         = 'HITRAN/LBLRTM absorber ID number for atmospheric absorbers'
  ABSORBER_UNITS_ID_DESCRIPTION   = 'LBLRTM/MonoRTM absorber units ID number'
  ABSORBER_UNITS_NAME_DESCRIPTION = 'Absorber Units Name'
  LEVEL_PRESSURE_DESCRIPTION      = 'Level pressure'
  LEVEL_TEMPERATURE_DESCRIPTION   = 'Level temperature'
  LEVEL_ABSORBER_DESCRIPTION      = 'Level absorber amount'
  LEVEL_ALTITUDE_DESCRIPTION      = 'Level geopotential altitude'
  LAYER_PRESSURE_DESCRIPTION      = 'Average layer pressure'
  LAYER_TEMPERATURE_DESCRIPTION   = 'Average layer temperature'
  LAYER_ABSORBER_DESCRIPTION      = 'Average layer absorber amount'
  LAYER_DELTA_Z_DESCRIPTION       = 'Layer thickness'

  ; Variable units attribute.
  UNITS_ATTNAME = 'units'

  DESCRIPTION_UNITS         = 'N/A'
  CLIMATOLOGY_MODEL_UNITS   = 'N/A'
  YEAR_UNITS                = 'Year (C.E.)'
  MONTH_UNITS               = 'Month of year'
  DAY_UNITS                 = 'Day of month'
  HOUR_UNITS                = 'Hour of day'
  LATITUDE_UNITS            = 'degress North (-90->+90)'
  LONGITUDE_UNITS           = 'degress East (0->360)'
  SURFACE_ALTITUDE_UNITS    = 'metres (m)'
  ABSORBER_ID_UNITS         = 'N/A'
  ABSORBER_UNITS_ID_UNITS   = 'N/A'
  ABSORBER_UNITS_NAME_UNITS = 'N/A'
  LEVEL_PRESSURE_UNITS      = 'hectoPascals (hPa)'
  LEVEL_TEMPERATURE_UNITS   = 'Kelvin (K)'
  LEVEL_ABSORBER_UNITS      = 'Variable (see Absorber_Units_Name)'
  LEVEL_ALTITUDE_UNITS      = 'metres (m)'
  LAYER_PRESSURE_UNITS      = 'hectoPascals (hPa)'
  LAYER_TEMPERATURE_UNITS   = 'Kelvin (K)'
  LAYER_ABSORBER_UNITS      = 'Variable (see Absorber_Units_Name)'
  LAYER_DELTA_Z_UNITS       = 'metres (m)'

  ; Variable fill value attribute
  FILLVALUE_ATTNAME = '_fillvalue'

  DESCRIPTION_FILLVALUE           = ' '
  CLIMATOLOGY_MODEL_FILLVALUE     = INVALID_CLIMATOLOGY
  YEAR_FILLVALUE                  = IP_INVALID
  MONTH_FILLVALUE                 = IP_INVALID
  DAY_FILLVALUE                   = IP_INVALID
  HOUR_FILLVALUE                  = IP_INVALID
  LATITUDE_FILLVALUE              = FP_INVALID
  LONGITUDE_FILLVALUE             = FP_INVALID
  SURFACE_ALTITUDE_FILLVALUE      = ZERO
  ABSORBER_ID_FILLVALUE           = 0L
  ABSORBER_UNITS_ID_FILLVALUE     = 0L
  ABSORBER_UNITS_NAME_FILLVALUE   = ' '
  LEVEL_PRESSURE_FILLVALUE        = FP_INVALID
  LEVEL_TEMPERATURE_FILLVALUE     = FP_INVALID
  LEVEL_ABSORBER_FILLVALUE        = FP_INVALID
  LEVEL_ALTITUDE_FILLVALUE        = FP_INVALID
  LAYER_PRESSURE_FILLVALUE        = FP_INVALID
  LAYER_TEMPERATURE_FILLVALUE     = FP_INVALID
  LAYER_ABSORBER_FILLVALUE        = FP_INVALID
  LAYER_DELTA_Z_FILLVALUE         = FP_INVALID

;-
