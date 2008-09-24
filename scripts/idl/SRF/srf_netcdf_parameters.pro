;+
; Parameters for SRF netCDF I/O routines

  @srf_parameters
  
  ; Global attribute names. Case sensitive
  TITLE_GATTNAME            = 'title' 
  HISTORY_GATTNAME          = 'history' 
  COMMENT_GATTNAME          = 'comment' 
  RELEASE_GATTNAME          = 'Release'
  VERSION_GATTNAME          = 'Version'
  SENSOR_ID_GATTNAME        = 'Sensor_Id'
  WMO_SATELLITE_ID_GATTNAME = 'WMO_Satellite_Id'
  WMO_SENSOR_ID_GATTNAME    = 'WMO_Sensor_Id'
  
  ; Dimension names. Case sensitive
  CHANNEL_DIMNAME = 'n_Channels'

  ; Variable names. Case sensitive.
  SENSOR_TYPE_VARNAME      = 'Sensor_Type'
  SENSOR_CHANNEL_VARNAME   = 'Sensor_Channel'
  INTEGRATED_SRF_VARNAME   = 'Integrated_SRF'
  SUMMATION_SRF_VARNAME    = 'Summation_SRF'

 
  ; Variable long name attribute.
  LONGNAME_ATTNAME = 'long_name'

  SENSOR_TYPE_LONGNAME    = 'Sensor Type'
  SENSOR_CHANNEL_LONGNAME = 'Sensor Channel'
  INTEGRATED_SRF_LONGNAME = 'Integrated SRF value'
  SUMMATION_SRF_LONGNAME  = 'Summed SRF value'
  F1_BAND_LONGNAME        = 'Band Begin Frequency'
  F2_BAND_LONGNAME        = 'Band End Frequency'
  NPTS_BAND_LONGNAME      = 'Number of band spectral points'
  FREQUENCY_LONGNAME      = 'Frequency'
  RESPONSE_LONGNAME       = 'Relative Response'


  ; Variable description attribute.
  DESCRIPTION_ATTNAME = 'description'
  
  SENSOR_TYPE_DESCRIPTION    = 'Sensor type to identify uW, IR, VIS, UV, etc sensor channels'
  SENSOR_CHANNEL_DESCRIPTION = 'List of sensor channel numbers'
  INTEGRATED_SRF_DESCRIPTION = 'SRF integral using Simpsons rule'
  SUMMATION_SRF_DESCRIPTION  = 'SRF integral using SUM(response)*dF'
  F1_BAND_DESCRIPTION        = 'Band Begin Frequency'
  F2_BAND_DESCRIPTION        = 'Band End Frequency'
  NPTS_BAND_DESCRIPTION      = 'Number of spectral points in a band'
  FREQUENCY_DESCRIPTION      = 'Spectral ordinate for channel responses'
  RESPONSE_DESCRIPTION       = 'Relative Spectral Response Function (SRF)'


  ; Variable units attribute.
  UNITS_ATTNAME = 'units'
  
  SENSOR_TYPE_UNITS    = 'N/A'
  SENSOR_CHANNEL_UNITS = 'N/A'
  NPTS_BAND_UNITS      = 'N/A' 
  INTEGRAL_SRF_UNITS   = 'N/A'
  RESPONSE_UNITS       = 'N/A'


  ; Variable _FillValue attribute.
  FILLVALUE_ATTNAME = '_FillValue'

  SENSOR_TYPE_FILLVALUE    = INVALID_SENSOR
  SENSOR_CHANNEL_FILLVALUE = INVALID
  NPTS_BAND_FILLVALUE      = INVALID
  INTEGRAL_SRF_FILLVALUE   = ZERO
  FREQUENCY_FILLVALUE      = -ONE
  RESPONSE_FILLVALUE       = -ONE
