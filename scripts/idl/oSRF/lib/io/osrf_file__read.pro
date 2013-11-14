;+
; NAME:
;       OSRF_File::Read
;
; PURPOSE:
;       The OSRF_File::Read procedure method reads an OSRF_File and fills the
;       container with all the OSRF objects in the file.
;
; CALLING SEQUENCE:
;       Obj->[OSRF_File::]Read, $
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
;
;-

; Helper procedure
PRO Print_Header, sensor_id, channel
  COMPILE_OPT HIDDEN
  PRINT, sensor_id, channel, $
         FORMAT='(//,50("="),/2x,"Sensor id : ",a,", Channel : ",i4,/,50("="))'
END


; Main procedure
PRO OSRF_File::Read, $
  Debug = Debug     ; Input keyword


  ; Set up
  COMPILE_OPT HIDDEN
  ; ...netCDF parameters
  @osrf_file_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler
  
  
  ; Check the file exists
  fInfo = FILE_INFO(self.filename)
  IF ( NOT fInfo.EXISTS ) THEN $
    MESSAGE, self.filename+' not found', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Open the file for reading
  fid = NCDF_OPEN( self.filename, /NOWRITE )
  NCDF_CONTROL, fid, /VERBOSE
  
  
  ; Get the number of channels dimension
  DimID = NCDF_DIMID( fid, CHANNEL_DIMNAME )
  NCDF_DIMINQ, fid, DimID, DimName, n_Channels

  
  ; Read the global attributes
  self->Read_GAtts, Debug = Debug
  ; ...Pull out some for setting by oSRF 
  self->Get_Property, $
    Debug            = Debug           , $
    Sensor_Id        = Sensor_Id       , $
    WMO_Satellite_Id = WMO_Satellite_Id, $
    WMO_Sensor_Id    = WMO_Sensor_Id   , $
    Sensor_Type      = Sensor_Type     
  
  
  ; Loop over the number of channels
  FOR n = 0L, n_Channels-1L DO BEGIN
  

    ; Read the channel number
    VarId = NCDF_VARID(fid, SENSOR_CHANNEL_VARNAME)
    NCDF_VARGET, fid, VarId, Channel, OFFSET=n, COUNT=1
    
    
    ; Read the band dimension
    self->Create_Names, $
      Channel, $
      Debug             = Debug            , $
      n_Bands_DimName   = n_Bands_DimName
    n_Bands_DimId = NCDF_DIMID( fid, n_Bands_DimName )
    NCDF_DIMINQ, fid, n_Bands_DimId, DimName, n_Bands


    ; Create the remaining dimension and variable names
    self->Create_Names, $
      Channel, $
      n_Bands, $
      Debug             = Debug            , $
      n_Points_DimName  = n_Points_DimName , $
      f1_VarName        = f1_VarName       , $
      f2_VarName        = f2_VarName       , $
      Frequency_VarName = Frequency_VarName, $
      Response_VarName  = Response_VarName
  
  
    ; Get the number of points for each band
    n_Points_DimId = LONARR(n_Bands)
    n_Points = LONARR(n_Bands)
    FOR i = 0L, n_Bands-1L DO BEGIN
      n_Points_DimId[i] = NCDF_DIMID( fid, n_Points_DimName[i] )
      NCDF_DIMINQ, fid, n_Points_DimName[i], DimName, nbp
      n_Points[i] = nbp
    ENDFOR


    ; Allocate the current OSRF
    osrf = OBJ_NEW('OSRF', Debug = Debug)
    osrf->Allocate, n_Points, Debug = Debug


    ; Set the global attribute properties for the current oSRF
    osrf->OSRF::Set_Property, $
      Debug = Debug, $
      Sensor_Id        = Sensor_Id       , $
      WMO_Satellite_ID = WMO_Satellite_ID, $
      WMO_Sensor_ID    = WMO_Sensor_ID   , $
      Sensor_Type      = Sensor_Type
      
    
    ; Read the channel dependent data
    ; ...The sensor channel
    osrf->OSRF::Set_Property, $
      Debug = Debug, $
      Channel = Channel
    ; ...The processing flags
    VarId = NCDF_VARID( fid, FLAGS_VARNAME )
    NCDF_VARGET, fid, VarId, expected_flags, OFFSET = n, COUNT = 1
    ; ...The integrated SRF value
    VarId = NCDF_VARID( fid, INTEGRATED_SRF_VARNAME )
    NCDF_VARGET, fid, VarId, expected_integral, OFFSET = n, COUNT = 1
    ; ...The central frequency
    VarId = NCDF_VARID( fid, CENTRAL_FREQUENCY_VARNAME )
    NCDF_VARGET, fid, VarId, expected_f0, OFFSET = n, COUNT = 1
    ; ...The Planck coefficients
    VarId = NCDF_VARID( fid, PLANCK_COEFFS_VARNAME )
    NCDF_VARGET, fid, VarId, expected_planck_coeffs, OFFSET = [0,n], COUNT = [N_PLANCK_COEFFS,1]
    ; ...The Polychromatic correction coefficients
    VarId = NCDF_VARID( fid, POLYCHROMATIC_COEFFS_VARNAME )
    NCDF_VARGET, fid, VarId, expected_polychromatic_coeffs, OFFSET = [0,n], COUNT = [N_POLYCHROMATIC_COEFFS,1]


    ; Read the band dependent data
    FOR i = 0L, n_Bands-1L DO BEGIN
      Band = i+1L
      ; Get the variable ids
      Frequency_VarId = NCDF_VARID( fid, Frequency_VarName[i] )
      Response_VarId  = NCDF_VARID( fid, Response_VarName[i] )
      ; Get the data
      NCDF_VARGET, fid, Frequency_VarId, f
      NCDF_VARGET, fid, Response_VarId , r
      ; and save it
      osrf->OSRF::Set_Property, $
        Band, $
        Debug = Debug, $
        Frequency = f, Response = r
    ENDFOR


    ; Process the oSRF
    osrf->OSRF::Integrate, Debug=Debug
    osrf->OSRF::Compute_Central_Frequency, Debug=Debug
    osrf->OSRF::Compute_Planck_Coefficients, Debug=Debug
    osrf->OSRF::Compute_Polychromatic_Coefficients, Debug=Debug


    ; Compare the just calculated values with those read from file
    output_header = TRUE
    ; ...The bit flags
    osrf->OSRF::Get_Property, Flags=actual_flags, Debug=Debug
    difference = actual_flags-expected_flags
    IF ( difference NE 0 ) THEN BEGIN
      IF ( output_header ) THEN BEGIN
        Print_Header, sensor_id, channel
        output_header = FALSE
      ENDIF
      MESSAGE, 'Processing bit flags are different from value read from file.',/INFORMATIONAL
      PRINT, actual_flags, expected_flags, $
             difference, $
             FORMAT='(2x,"Actual     : ",b32.32,'+$
                    '/2x,"Expected   : ",b32.32,'+$
                    '/2x,"Difference : ",b32.32,/)'
    ENDIF
    ; ...The integrated value
    osrf->OSRF::Get_Property, Integral=actual_integral, Debug=Debug
    difference = ABS(actual_integral-expected_integral)
    IF ( difference GT THRESHOLD ) THEN BEGIN
      IF ( output_header ) THEN BEGIN
        Print_Header, sensor_id, channel
        output_header = FALSE
      ENDIF
      MESSAGE, 'Computed integral is different from value read from file.',/INFORMATIONAL
      PRINT, actual_integral, expected_integral, $
             difference, THRESHOLD, $
             FORMAT='(2x,"Actual     : ",f26.20,'+$
                    '/2x,"Expected   : ",f26.20,'+$
                    '/2x,"Difference : ",e13.6,'+$
                    '/2x,"Threshold  : ",e13.6,/)'
    ENDIF
    ; ...The central frequency
    osrf->OSRF::Get_Property, f0=actual_f0, Debug=Debug
    difference = ABS(actual_f0-expected_f0)
    IF ( difference GT THRESHOLD ) THEN BEGIN
      IF ( output_header ) THEN BEGIN
        Print_Header, sensor_id, channel
        output_header = FALSE
      ENDIF
      MESSAGE, 'Computed f0 is different from value read from file.',/INFORMATIONAL
      PRINT, actual_f0, expected_f0, $
             difference, THRESHOLD, $
             FORMAT='(2x,"Actual     : ",f26.20,'+$
                    '/2x,"Expected   : ",f26.20,'+$
                    '/2x,"Difference : ",e13.6,'+$
                    '/2x,"Threshold  : ",e13.6,/)'
    ENDIF
    ; ...The Planck coefficients
    osrf->OSRF::Get_Property, Planck_Coeffs=actual_planck_coeffs, Debug=Debug
    difference = ABS(actual_planck_coeffs-expected_planck_coeffs)
    loc = WHERE(difference GT threshold, count)
    IF ( count GT 0 ) THEN BEGIN
      IF ( output_header ) THEN BEGIN
        Print_Header, sensor_id, channel
        output_header = FALSE
      ENDIF
      MESSAGE, 'Computed Planck coefficients are different from values read from file.',/INFORMATIONAL
      PRINT, actual_planck_coeffs, expected_planck_coeffs, $
             difference, THRESHOLD, THRESHOLD, $
             FORMAT='(2x,"Actual     : ",'+STRTRIM(N_PLANCK_COEFFS,2)+'(1x,e26.20),'+$
                    '/2x,"Expected   : ",'+STRTRIM(N_PLANCK_COEFFS,2)+'(1x,e26.20),'+$
                    '/2x,"Difference : ",'+STRTRIM(N_PLANCK_COEFFS,2)+'(1x,e13.6),'+$
                    '/2x,"Threshold  : ",'+STRTRIM(N_PLANCK_COEFFS,2)+'(1x,e13.6),/)'
    ENDIF
    ; ...The band correction coefficients coefficients
    osrf->OSRF::Get_Property, Polychromatic_Coeffs=actual_polychromatic_coeffs, Debug=Debug
    difference = ABS(actual_polychromatic_coeffs-expected_polychromatic_coeffs)
    loc = WHERE(difference GT threshold, count)
    IF ( count GT 0 ) THEN BEGIN
      IF ( output_header ) THEN BEGIN
        Print_Header, sensor_id, channel
        output_header = FALSE
      ENDIF
      MESSAGE, 'Computed band correction coefficients are different from values read from file.',/INFORMATIONAL
      PRINT, actual_polychromatic_coeffs, expected_polychromatic_coeffs, $
             difference, THRESHOLD, THRESHOLD, $
             FORMAT='(2x,"Actual     : ",'+STRTRIM(N_POLYCHROMATIC_COEFFS,2)+'(1x,f26.20),'+$
                    '/2x,"Expected   : ",'+STRTRIM(N_POLYCHROMATIC_COEFFS,2)+'(1x,f26.20),'+$
                    '/2x,"Difference : ",'+STRTRIM(N_POLYCHROMATIC_COEFFS,2)+'(1x,e13.6),'+$
                    '/2x,"Threshold  : ",'+STRTRIM(N_POLYCHROMATIC_COEFFS,2)+'(1x,e13.6),/)'
    ENDIF
    
    
        
    ; Add the current OSRF to the file object
    self->Add, osrf


  ENDFOR
  NCDF_CLOSE, fid


  ; Done
  CATCH, /CANCEL

END ; PRO OSRF_File::Read
