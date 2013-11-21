;+
; Parameters for OSRF file I/O routines

  @osrf_parameters
  
  ; Global attribute names. Case sensitive
  TITLE_GATTNAME            = 'title' 
  HISTORY_GATTNAME          = 'history' 
  COMMENT_GATTNAME          = 'comment' 
  RELEASE_GATTNAME          = 'Release'
  VERSION_GATTNAME          = 'Version'
  SENSOR_ID_GATTNAME        = 'Sensor_Id'
  SENSOR_TYPE_GATTNAME      = 'Sensor_Type'
  WMO_SATELLITE_ID_GATTNAME = 'WMO_Satellite_Id'
  WMO_SENSOR_ID_GATTNAME    = 'WMO_Sensor_Id'
  
  ; Dimension names. Case sensitive
  CHANNEL_DIMNAME              = 'n_Channels'
  PLANCK_COEFFS_DIMNAME        = 'n_Planck_Coeffs'
  POLYCHROMATIC_COEFFS_DIMNAME = 'n_Polychromatic_Coeffs'
  TEMPERATURE_DIMNAME          = 'n_Temperatures'

  ; Variable names. Case sensitive.
  SENSOR_CHANNEL_VARNAME       = 'Sensor_Channel'
  INTEGRATED_SRF_VARNAME       = 'Integrated_SRF'
  FLAGS_VARNAME                = 'Flags'
  CENTRAL_FREQUENCY_VARNAME    = 'Central_Frequency'
  PLANCK_COEFFS_VARNAME        = 'Planck_Coeffs'
  POLYCHROMATIC_COEFFS_VARNAME = 'Polychromatic_Coeffs'

 
  ; Variable long name attribute.
  LONGNAME_ATTNAME = 'long_name'

  SENSOR_CHANNEL_LONGNAME       = 'Sensor Channel'
  INTEGRATED_SRF_LONGNAME       = 'Integrated SRF value'
  FLAGS_LONGNAME                = 'Processing Bit Flags'
  CENTRAL_FREQUENCY_LONGNAME    = 'Central Frequency'
  PLANCK_COEFFS_LONGNAME        = 'Planck Coefficients'
  POLYCHROMATIC_COEFFS_LONGNAME = 'Polychromatic Correction Coefficients'
  T_LONGNAME                    = 'Temperature'
  TEFF_LONGNAME                 = 'Effective Temperature'
  TFIT_LONGNAME                 = 'Fit to Effective Temperature'
  F1_LONGNAME                   = 'Band Begin Frequency'
  F2_LONGNAME                   = 'Band End Frequency'
  N_POINTS_LONGNAME             = 'Number of band spectral points'
  FREQUENCY_LONGNAME            = 'Band Frequency'
  RESPONSE_LONGNAME             = 'Band Relative Response'


  ; Variable description attribute.
  DESCRIPTION_ATTNAME = 'description'
  
  SENSOR_CHANNEL_DESCRIPTION       = 'List of sensor channel numbers'
  INTEGRATED_SRF_DESCRIPTION       = 'Integral of SRF for normalisation'
  FLAGS_DESCRIPTION                = 'Bit Flags set during SRF processing'
  CENTRAL_FREQUENCY_DESCRIPTION    = 'First moment of the SRF'
  PLANCK_COEFFS_DESCRIPTION        = 'Planck function coefficients'
  POLYCHROMATIC_COEFFS_DESCRIPTION = 'Correction coefficients for non-monochromatic bandwidths'
  T_DESCRIPTION                    = 'True Temperature'
  TEFF_DESCRIPTION                 = 'Effective temperature due to band polychromaticity'
  TFIT_DESCRIPTION                 = 'Polynomial fit to Teff=f(T) data'
  F1_DESCRIPTION                   = 'Band Begin Frequency'
  F2_DESCRIPTION                   = 'Band End Frequency'
  N_POINTS_DESCRIPTION             = 'Number of spectral points in a band'
  FREQUENCY_DESCRIPTION            = 'Spectral ordinate for channel responses'
  RESPONSE_DESCRIPTION             = 'Relative Spectral Response Function (SRF)'


  ; Variable units attribute.
  UNITS_ATTNAME = 'units'
  
  SENSOR_CHANNEL_UNITS       = 'N/A'
  NPTS_BAND_UNITS            = 'N/A' 
  INTEGRATED_SRF_UNITS       = 'N/A'
  FLAGS_UNITS                = 'N/A'
  PLANCK_COEFFS_UNITS        = '[W.m^2, K.m]'
  POLYCHROMATIC_COEFFS_UNITS = '[K, K/K]'
  T_UNITS                    = 'Kelvin'
  FREQUENCY_UNITS            = SENSOR_FREQUENCY_UNITS ; From osrf_parameters include file
  RESPONSE_UNITS             = 'N/A'

  ; Variable _FillValue attribute.
  FILLVALUE_ATTNAME = '_FillValue'

  SENSOR_CHANNEL_FILLVALUE       = INVALID
  NPTS_BAND_FILLVALUE            = INVALID
  INTEGRATED_SRF_FILLVALUE       = ZERO
  FLAGS_FILLVALUE                = -1L
  PLANCK_COEFFS_FILLVALUE        = ZERO
  POLYCHROMATIC_COEFFS_FILLVALUE = ZERO
  T_FILLVALUE                    = ZERO
  FREQUENCY_FILLVALUE            = ZERO
  RESPONSE_FILLVALUE             = ZERO

;+
