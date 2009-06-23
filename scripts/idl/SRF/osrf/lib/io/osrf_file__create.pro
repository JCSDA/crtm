;
; NAME:
;       OSRF_File::Create
;
; PURPOSE:
;       The OSRF_File::Create procedure method creates an OSRF_File
;       for writing.
;
; CALLING SEQUENCE:
;       Obj->[OSRF_File::]Create, $
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
;       osrf_file parameters: Include file containing OSRF_File specific
;                             parameter value definitions.
;
;       osrf_pro_err_handler: Error handler code for OSRF procedures.
;
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 23-Jun-2009
;                       paul.vandelst@noaa.gov
;-

PRO OSRF_File::Create, $
  Debug = Debug
  
  ; Set up
  ; ...netCDF parameters
  @osrf_file_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler
  

  ; Create the data file
  fid = NCDF_CREATE(self.filename, /CLOBBER)
  NCDF_CONTROL, fid, /VERBOSE

  
  ; Define the known dimensions
  n_Planck_Coeffs_DimId        = NCDF_DIMDEF(fid, PLANCK_COEFFS_DIMNAME       , N_PLANCK_COEFFS)
  n_Polychromatic_Coeffs_DimId = NCDF_DIMDEF(fid, POLYCHROMATIC_COEFFS_DIMNAME, N_POLYCHROMATIC_COEFFS)
  n_Channels_DimId             = NCDF_DIMDEF(fid, CHANNEL_DIMNAME             , /UNLIMITED)

 
  ; Define the channel-dimensioned variables and attributes
  ; ...The Sensor_Channel
  Sensor_Channel_VarId = NCDF_VARDEF( fid, SENSOR_CHANNEL_VARNAME, n_Channels_DimId, /LONG)
  NCDF_ATTPUT, fid, Sensor_Channel_VarId, LONGNAME_ATTNAME   , SENSOR_CHANNEL_LONGNAME 
  NCDF_ATTPUT, fid, Sensor_Channel_VarId, DESCRIPTION_ATTNAME, SENSOR_CHANNEL_DESCRIPTION
  NCDF_ATTPUT, fid, Sensor_Channel_VarId, UNITS_ATTNAME      , SENSOR_CHANNEL_UNITS
  NCDF_ATTPUT, fid, Sensor_Channel_VarId, FILLVALUE_ATTNAME  , SENSOR_CHANNEL_FILLVALUE
  ; ...The Integrated_SRF
  VarId = NCDF_VARDEF(fid, INTEGRATED_SRF_VARNAME, n_Channels_DimId, /DOUBLE)
  NCDF_ATTPUT, fid, VarId, LONGNAME_ATTNAME   , INTEGRATED_SRF_LONGNAME 
  NCDF_ATTPUT, fid, VarId, DESCRIPTION_ATTNAME, INTEGRATED_SRF_DESCRIPTION
  NCDF_ATTPUT, fid, VarId, UNITS_ATTNAME      , INTEGRATED_SRF_UNITS
  NCDF_ATTPUT, fid, VarId, FILLVALUE_ATTNAME  , INTEGRATED_SRF_FILLVALUE
  ; ...The Flags
  VarId = NCDF_VARDEF(fid, FLAGS_VARNAME, n_Channels_DimId, /LONG)
  NCDF_ATTPUT, fid, VarId, LONGNAME_ATTNAME   , FLAGS_LONGNAME 
  NCDF_ATTPUT, fid, VarId, DESCRIPTION_ATTNAME, FLAGS_DESCRIPTION
  NCDF_ATTPUT, fid, VarId, UNITS_ATTNAME      , FLAGS_UNITS
  NCDF_ATTPUT, fid, VarId, FILLVALUE_ATTNAME  , FLAGS_FILLVALUE
  ; ...The Central_Frequency
  VarId = NCDF_VARDEF(fid, CENTRAL_FREQUENCY_VARNAME, n_Channels_DimId, /DOUBLE)
  NCDF_ATTPUT, fid, VarId, LONGNAME_ATTNAME   , CENTRAL_FREQUENCY_LONGNAME 
  NCDF_ATTPUT, fid, VarId, DESCRIPTION_ATTNAME, CENTRAL_FREQUENCY_DESCRIPTION
  NCDF_ATTPUT, fid, VarId, UNITS_ATTNAME      , FREQUENCY_UNITS[self.Sensor_Type]
  NCDF_ATTPUT, fid, VarId, FILLVALUE_ATTNAME  , FREQUENCY_FILLVALUE
  ; ...The Planck_Coeffs
  VarId = NCDF_VARDEF(fid, PLANCK_COEFFS_VARNAME, [n_Planck_Coeffs_DimId,n_Channels_DimId], /DOUBLE)
  NCDF_ATTPUT, fid, VarId, LONGNAME_ATTNAME   , PLANCK_COEFFS_LONGNAME 
  NCDF_ATTPUT, fid, VarId, DESCRIPTION_ATTNAME, PLANCK_COEFFS_DESCRIPTION
  NCDF_ATTPUT, fid, VarId, UNITS_ATTNAME      , PLANCK_COEFFS_UNITS
  NCDF_ATTPUT, fid, VarId, FILLVALUE_ATTNAME  , PLANCK_COEFFS_FILLVALUE
  ; ...The Polychromatic_Coeffs
  VarId = NCDF_VARDEF(fid, POLYCHROMATIC_COEFFS_VARNAME, [n_Polychromatic_Coeffs_DimId,n_Channels_DimId], /DOUBLE)
  NCDF_ATTPUT, fid, VarId, LONGNAME_ATTNAME   , POLYCHROMATIC_COEFFS_LONGNAME 
  NCDF_ATTPUT, fid, VarId, DESCRIPTION_ATTNAME, POLYCHROMATIC_COEFFS_DESCRIPTION
  NCDF_ATTPUT, fid, VarId, UNITS_ATTNAME      , POLYCHROMATIC_COEFFS_UNITS
  NCDF_ATTPUT, fid, VarId, FILLVALUE_ATTNAME  , POLYCHROMATIC_COEFFS_FILLVALUE


  ; Done
  NCDF_CLOSE, fid
  CATCH, /CANCEL

END ; PRO OSRF_File::Create

