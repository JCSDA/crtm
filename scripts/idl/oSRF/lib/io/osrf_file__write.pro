;+
; NAME:
;       OSRF_File::Write
;
; PURPOSE:
;       The OSRF_File::Write procedure method writes all the contained OSRF
;       objects to an OSRF_File
;
; CALLING SEQUENCE:
;       Obj->[OSRF_File::]Write, $
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
;       Written by:     Paul van Delst, 22-Jun-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO OSRF_File::Write, $
  Debug = Debug     ; Input keyword


  ; Set up
  COMPILE_OPT HIDDEN
  ; ...netCDF parameters
  @osrf_file_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler
  
  
  ; Get the OSRF references and check count
  osrf = self->Get(/ALL, ISA='OSRF', COUNT=n_osrfs)
  IF ( n_OSRFs EQ 0 ) THEN RETURN


  ; Create the file
  self->Create, Debug=Debug


  ; Open the file for writing 
  fid = NCDF_OPEN( self.filename, /WRITE )
  NCDF_CONTROL, fid, /VERBOSE
  
  
  ; Loop over contained OSRF objects
  FOR n = 0L, n_osrfs-1L DO BEGIN
  
  
    ; Load  file object properties from the first OSRF object
    IF ( n EQ 0 ) THEN BEGIN
      osrf[n]->OSRF::Get_Property, $
        Debug            = debug           , $
        Sensor_Id        = sensor_id       , $
        WMO_Satellite_Id = wmo_satellite_id, $
        WMO_Sensor_Id    = wmo_sensor_id   , $
        Sensor_Type      = sensor_type     
      self.Sensor_Id        = sensor_id       
      self.WMO_Satellite_Id = wmo_satellite_id
      self.WMO_Sensor_Id    = wmo_sensor_id   
      self.Sensor_Type      = sensor_type     
    ENDIF

  
    ; Create the SRF dimension and variable names for the current channel
    osrf[n]->OSRF::Get_Property, $
      Debug          = debug  , $
      Channel        = channel, $
      n_Bands        = n_bands, $
      n_Temperatures = n_temperatures
    self->Create_Names, $
      channel, $
      n_bands, $
      Debug                  = debug                 , $
      n_Bands_DimName        = n_bands_dimname       , $
      n_Temperatures_DimName = n_temperatures_dimname, $
      n_Points_DimName       = n_points_dimname      , $
      f1_VarName             = f1_varname            , $
      f2_VarName             = f2_varname            , $
      Frequency_VarName      = frequency_varname     , $
      Response_VarName       = response_varname      , $
      T_VarName              = t_varname             , $
      Teff_VarName           = teff_varname          , $
      Tfit_VarName           = tfit_varname          


    ; Put the file in define mode
    NCDF_CONTROL, fid, /REDEF

  
    ; Define the dimensions for this channel
    ; .. The number of bands
    n_bands_dimid  = NCDF_DIMDEF( fid, n_bands_dimname, n_bands )
    ; ...The number of points for each band
    n_points_dimid = LONARR(n_bands)
    FOR i = 0L, n_bands-1L DO BEGIN
      osrf[n]->OSRF::Get_Property, $
        i+1L, $
        Debug    = debug  , $
        n_Points = n_points
      n_points_dimid[i] = NCDF_DIMDEF( fid, n_points_dimname[i], n_points )
    ENDFOR
    ; .. The number of temperatures
    n_temperatures_dimid  = NCDF_DIMDEF( fid, n_temperatures_dimname, n_temperatures )

  
    ; Define the band variables
    ; ...The band begin frequency variable
    f1_varid = NCDF_VARDEF( fid, f1_varname, n_bands_dimid, /DOUBLE )
    NCDF_ATTPUT, fid, f1_varid, LONGNAME_ATTNAME   , F1_LONGNAME
    NCDF_ATTPUT, fid, f1_varid, DESCRIPTION_ATTNAME, F1_DESCRIPTION
    NCDF_ATTPUT, fid, f1_varid, UNITS_ATTNAME      , FREQUENCY_UNITS
    NCDF_ATTPUT, fid, f1_varid, FILLVALUE_ATTNAME  , FREQUENCY_FILLVALUE
    ; The band end frequency variable
    f2_varid = NCDF_VARDEF( fid, f2_varname, n_bands_dimid, /DOUBLE )
    NCDF_ATTPUT, fid, f2_varid, LONGNAME_ATTNAME   , F2_LONGNAME
    NCDF_ATTPUT, fid, f2_varid, DESCRIPTION_ATTNAME, F2_DESCRIPTION
    NCDF_ATTPUT, fid, f2_varid, UNITS_ATTNAME      , FREQUENCY_UNITS
    NCDF_ATTPUT, fid, f2_varid, FILLVALUE_ATTNAME  , FREQUENCY_FILLVALUE


    ; Define the temperature variables
    ; ...The true temperature
    t_varid = NCDF_VARDEF( fid, t_varname, n_temperatures_dimid, /DOUBLE )
    NCDF_ATTPUT, fid, t_varid, LONGNAME_ATTNAME   , T_LONGNAME
    NCDF_ATTPUT, fid, t_varid, DESCRIPTION_ATTNAME, T_DESCRIPTION
    NCDF_ATTPUT, fid, t_varid, UNITS_ATTNAME      , T_UNITS
    NCDF_ATTPUT, fid, t_varid, FILLVALUE_ATTNAME  , T_FILLVALUE
    ; ...The effective temperature
    teff_varid = NCDF_VARDEF( fid, teff_varname, n_temperatures_dimid, /DOUBLE )
    NCDF_ATTPUT, fid, teff_varid, LONGNAME_ATTNAME   , TEFF_LONGNAME
    NCDF_ATTPUT, fid, teff_varid, DESCRIPTION_ATTNAME, TEFF_DESCRIPTION
    NCDF_ATTPUT, fid, teff_varid, UNITS_ATTNAME      , T_UNITS
    NCDF_ATTPUT, fid, teff_varid, FILLVALUE_ATTNAME  , T_FILLVALUE
    ; ...The fit to the effective temperature
    tfit_varid = NCDF_VARDEF( fid, tfit_varname, n_temperatures_dimid, /DOUBLE )
    NCDF_ATTPUT, fid, tfit_varid, LONGNAME_ATTNAME   , TFIT_LONGNAME
    NCDF_ATTPUT, fid, tfit_varid, DESCRIPTION_ATTNAME, TFIT_DESCRIPTION
    NCDF_ATTPUT, fid, tfit_varid, UNITS_ATTNAME      , T_UNITS
    NCDF_ATTPUT, fid, tfit_varid, FILLVALUE_ATTNAME  , T_FILLVALUE


    ; Define the actual SRF variables
    Frequency_VarId = LONARR(n_Bands)
    Response_VarId  = LONARR(n_Bands)
    FOR i = 0L, n_Bands-1L DO BEGIN
      ; The frequency grid for this channel's band
      Frequency_VarId[i] = NCDF_VARDEF( fid, Frequency_VarName[i], n_Points_DimId[i], /DOUBLE )
      NCDF_ATTPUT, fid, Frequency_VarId[i], LONGNAME_ATTNAME   , FREQUENCY_LONGNAME 
      NCDF_ATTPUT, fid, Frequency_VarId[i], DESCRIPTION_ATTNAME, FREQUENCY_DESCRIPTION
      NCDF_ATTPUT, fid, Frequency_VarId[i], UNITS_ATTNAME      , FREQUENCY_UNITS
      NCDF_ATTPUT, fid, Frequency_VarId[i], FILLVALUE_ATTNAME  , FREQUENCY_FILLVALUE
      ; The response data for this channel's band
      Response_VarId[i] = NCDF_VARDEF( fid, Response_VarName[i], n_Points_DimId[i], /DOUBLE )
      NCDF_ATTPUT, fid, Response_VarId[i], LONGNAME_ATTNAME   , RESPONSE_LONGNAME 
      NCDF_ATTPUT, fid, Response_VarId[i], DESCRIPTION_ATTNAME, RESPONSE_DESCRIPTION
      NCDF_ATTPUT, fid, Response_VarId[i], UNITS_ATTNAME      , RESPONSE_UNITS
      NCDF_ATTPUT, fid, Response_VarId[i], FILLVALUE_ATTNAME  , RESPONSE_FILLVALUE
    ENDFOR

  
    ; Put the file into data mode 
    NCDF_CONTROL, fid, /ENDEF


    ; Write the channel dependent data
    ; ...The sensor channel
    osrf[n]->OSRF::Get_Property, $
      Debug = Debug, $
      Channel = Channel
    VarId = NCDF_VARID( fid, SENSOR_CHANNEL_VARNAME )
    NCDF_VARPUT, fid, VarId, Channel, OFFSET = n
    ; ...The integrated SRF value
    osrf[n]->OSRF::Get_Property, $
      Debug = Debug, $
      Integral = Integral
    VarId = NCDF_VARID( fid, INTEGRATED_SRF_VARNAME )
    NCDF_VARPUT, fid, VarId, Integral, OFFSET = n
    ; ...The processing flags
    osrf[n]->OSRF::Get_Property, $
      Debug = Debug, $
      Flags = Flags
    VarId = NCDF_VARID( fid, FLAGS_VARNAME )
    NCDF_VARPUT, fid, VarId, Flags, OFFSET = n
    ; ...The central frequency
    osrf[n]->OSRF::Get_Property, $
      Debug = Debug, $
      f0 = f0
    VarId = NCDF_VARID( fid, CENTRAL_FREQUENCY_VARNAME )
    NCDF_VARPUT, fid, VarId, f0, OFFSET = n
    ; ...The Planck coefficients
    osrf[n]->OSRF::Get_Property, $
      Debug = Debug, $
      Planck_Coeffs = Planck_Coeffs
    VarId = NCDF_VARID( fid, PLANCK_COEFFS_VARNAME )
    NCDF_VARPUT, fid, VarId, Planck_Coeffs, OFFSET = [0,n]
    ; ...The Polychromatic correction coefficients
    osrf[n]->OSRF::Get_Property, $
      Debug = Debug, $
      Polychromatic_Coeffs = Polychromatic_Coeffs
    VarId = NCDF_VARID( fid, POLYCHROMATIC_COEFFS_VARNAME )
    NCDF_VARPUT, fid, VarId, Polychromatic_Coeffs, OFFSET = [0,n]
    ; ...The polychromatic temperature data
    osrf[n]->OSRF::Get_Property, $
      Debug = Debug, $
      poly_Tdata = poly_tdata
    NCDF_VARPUT, fid, t_varid   , poly_tdata["T"]
    NCDF_VARPUT, fid, teff_varid, poly_tdata["Teff"]
    NCDF_VARPUT, fid, tfit_varid, poly_tdata["Tfit"]


    ; Write the band dependent data
    FOR i = 0L, n_Bands-1L DO BEGIN
      Band = i+1L
      ; Write the band frequency limits
      osrf[n]->OSRF::Get_Property, $
        Band, $
        Debug = Debug, $
        f1 = f1, f2 = f2
      NCDF_VARPUT, fid, f1_VarId, f1, OFFSET=i
      NCDF_VARPUT, fid, f2_VarId, f2, OFFSET=i
      ; The SRF data
      osrf[n]->OSRF::Get_Property, $
        Band, $
        Debug = Debug, $
        Frequency = f, Response = r
      NCDF_VARPUT, fid, Frequency_VarId[i], f
      NCDF_VARPUT, fid, Response_VarId[i] , r
    ENDFOR

  ENDFOR
  NCDF_CLOSE, fid


  ; Write the global attributes
  self->Write_GAtts, Debug=Debug
  
END
