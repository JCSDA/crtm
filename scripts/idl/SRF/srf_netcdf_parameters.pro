  ; Global attribute names. Case sensitive
  TITLE_GATTNAME         = 'title' 
  HISTORY_GATTNAME       = 'history' 
  SENSOR_NAME_GATTNAME   = 'sensor_name' 
  PLATFORM_NAME_GATTNAME = 'platform_name' 
  COMMENT_GATTNAME       = 'comment' 

  ; Static dimension names. Case sensitive
  CHANNEL_DIMNAME = 'n_channels'

  ; Static variable names. Case sensitive.
  WMO_SATELLITE_ID_VARNAME = 'WMO_Satellite_ID'
  WMO_SENSOR_ID_VARNAME    = 'WMO_Sensor_ID'
  CHANNEL_LIST_VARNAME     = 'channel_list'
  BEGIN_FREQUENCY_VARNAME  = 'begin_frequency'
  END_FREQUENCY_VARNAME    = 'end_frequency'
  INTEGRATED_SRF_VARNAME   = 'integrated_srf'
  SUMMATION_SRF_VARNAME    = 'summation_srf'

  ; Variable long name attribute.
  LONGNAME_ATTNAME = 'long_name'
  WMO_SATELLITE_ID_LONGNAME = 'WMO code for identifying satellite platforms (1023 == none available)'
  WMO_SENSOR_ID_LONGNAME    = 'WMO code for identifying a satellite sensor (2047 == none available)'
  CHANNEL_LIST_LONGNAME     = 'List of sensor channel numbers associated with the SRF data'
  BEGIN_FREQUENCY_LONGNAME  = 'Begin frequencies of SRF response data'
  END_FREQUENCY_LONGNAME    = 'End frequencies of SRF response data'
  INTEGRATED_SRF_LONGNAME   = 'Integrated spectral response using Simpsons rule'
  SUMMATION_SRF_LONGNAME    = 'Integrated spectral response by summation: = SUM( response ) * df'

  ; Variable units attribute.
  UNITS_ATTNAME = 'units'
  WMO_SATELLITE_ID_UNITS = 'N/A'
  WMO_SENSOR_ID_UNITS    = 'N/A'
  CHANNEL_LIST_UNITS     = 'N/A'
  FREQUENCY_UNITS        = 'Inverse centimetres (cm^-1)'
  RESPONSE_UNITS         = 'N/A'
  INTEGRATED_SRF_UNITS   = 'N/A'
  SUMMATION_SRF_UNITS    = 'N/A'

  ; Variable _FillValue attribute.
  FILLVALUE_ATTNAME = '_FillValue'
  WMO_SATELLITE_ID_FILLVALUE  = 1023L
  WMO_SENSOR_ID_FILLVALUE     = 2047L
  CHANNEL_LIST_FILLVALUE      = 0L
  FREQUENCY_FILLVALUE         = -1.0d0
  RESPONSE_FILLVALUE          = -1.0d0
  INTEGRATED_SRF_FILLVALUE    = -1.0d0
  SUMMATION_SRF_FILLVALUE     = -1.0d0
