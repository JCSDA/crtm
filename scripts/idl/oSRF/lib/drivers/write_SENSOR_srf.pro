; ########################################################################################################
;
; This file is a template for drivers that write oSRF netCDF files from raw SRF data
; that is provided in ascii files
;
; ============
; INSTRUCTIONS:
;
; 1) First "svn copy" this file from its generic name to a sensor specific name.
;    The drivers are copied to $CRTM_FIXFILE_ROOT/TauProd/SRF_Data/<sensor>
;    For example, for the VIIRS sensor:
;      $ svn copy write_SENSOR_srf.pro $CRTM_FIXFILE_ROOT/TauProd/SRF_Data/Raw/viirs/write_viirs_srf.pro
;
; 2) Set the svn:keywords property in the copied file.  For example, for the
;    VIIRS sensor:
;      $ svn propset svn:keywords "Id Revision" write_viirs_srf.pro
;
; 3) Do a global replace of the string "<sensor>" with the sensor name for which
;    the SRF driver is required.  (Double-check the result)
;
; 4) Modify the driver actions as needed for the sensor. Commented lines of code 
;    that are sensor specific are highlighted with * symbols in the left margin
;    and numbered. 
;
; 5) UPDATE THE HEADER DOCUMENTATION FOR THE CONTAINED PROCEDURE AS NECESSARY!!
;
; 6) Delete this template information section (between the "####"'s)
;
; 7) Commit the file to the repository.
;
; ########################################################################################################
;   
; NAME:
;       write_<sensor>_srf
; 
; PURPOSE:
;       This proceedure is a driver for writing oSRF 
;       netCDF files from raw <sensor> SRF data
; 
; CALLING SEQUENCE:
;       write_<sensor>_srf, $
;         Sensor_Id                               , $ ; Input
;         Path               = Path               , $ ; Input keyword, If not specified then Sensor_id
;         SensorInfo_File    = SensorInfo_File    , $ ; Input keyword, If not specified then 'SensorInfo'
;         Response_Threshold = Response_Threshold , $ ; Input keyword, If not specified then no threshold is applied
;         Version            = Version            , $ ; Input keyword, If not specified, default is OSRF_VERSION
;         No_Plot            = No_Plot            , $ ; Input keyword, If set plotting in driver turned off
;         No_Pause           = No_Pause           , $ ; Input keyword, If set then no pause between plots for channel
;         No_Threshold_Plot  = No_Threshold_Plot  , $ ; Input keyword, If set then plotting from apply_threshold method turned off
;         Debug              = Debug              , $ ; Input keyword, If set debug error messaging is turned on
;         gRef               = gRef               , $ ; Output keyword, If set a list of gRef object graphics is returned
;         tRef               = tRef                   ; Output keyword, If set a list of threshold graphics is returned
;
;
; INPUTS:
;       Sensor_Id:          The Sensor_Id of the oSRF file to be written
;                           UNITS:      N/A
;                           TYPE:       CHARACTER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(IN)
;
; INPUT KEYWORDS:
;
;       Path:               Specify this keyword if files are not located in directory Sensor_Id
;                           UNITS:      N/A
;                           TYPE:       CHARACTER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(IN)
;
;       SensorInfo_File:    Specify this keyword if a file is to be used in lieu of 'SensorInfo'
;                           UNITS:      N/A
;                           TYPE:       CHARACTER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(IN)
;         
;       Response_Threshold: Specify this keyword to apply a response threshold
;                           UNITS:      N/A
;                           TYPE:       DOUBLE
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(IN)
;
;       Version:            Specify this keyword if version is something other than OSRF_VERSION
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(IN)
;
;       No_Plot:            Set this keyword to turn off plotting in main routine
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(IN)       
;                  
;       No_Pause:           Set this keyword to turn off pause between plots
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(IN)
;
;       No_Threshold_Plot:  Set this to turn off threshold plotting
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(IN)
;
;       Debug:              Set this keyword for debugging. If set then:     
;                           - the error handler for this function is disabled
;                             so that execution halts where the error occurs,
;                           - more verbose output is produced.               
;                           UNITS:      N/A                                  
;                           TYPE:       INTEGER                              
;                           DIMENSION:  Scalar                               
;                           ATTRIBUTES: INTENT(IN), OPTIONAL                 
;
; OUTPUT KEYWORDS:
;       
;       gRef:               Set this keyword to return main output graphics
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       tRef:               Set this keyword to to return threshold output graphics
;                           UNITS:      N/A
;                           TYPE:       INTEGER  
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(IN), OPTIONAL
;
; INCLUDE FILES:
;       osrf_pro_err_handler: Error handler code for oSRF procedures.
;
;
;-

PRO Write_<sensor>_SRF, $
  Sensor_Id   , $ ; Input
  Path               = Path               , $ ; Input keyword
  SensorInfo_File    = SensorInfo_File    , $ ; Input keyword
  Response_Threshold = Response_Threshold , $ ; Input keyword
  Version            = Version            , $ ; Input keyword
  No_Plot            = No_Plot            , $ ; Input keyword
  No_Pause           = No_Pause           , $ ; Input keyword
  No_Threshold_Plot  = No_Threshold_Plot  , $ ; Input keyword
  Debug              = Debug              , $ ; Input keyword
  gRef               = gRef               , $ ; Output keyword
  tRef               = tRef                   ; Output keyword
;-  
  
  ; set osrf parameters
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler
  ; ...Color definitions 
  @color_db
  ; ...Check keywords
  Path = Valid_String(Path) ? Path : Sensor_Id
  SensorInfo_File = Valid_String(SensorInfo_File) ? SensorInfo_File : "SensorInfo"
  Apply_Threshold = (N_ELEMENTS(Response_Threshold) GT 0) ? TRUE : FALSE
  Version = (N_ELEMENTS(Version) GT 0) ? Version[0] : OSRF_VERSION
  Zero_Negative = NOT KEYWORD_SET(No_Zero_Negative)
  Plot_Data     = NOT KEYWORD_SET(No_Plot)
  Plot_Pause    = NOT KEYWORD_SET(No_Pause)
  
  ; ...Create list for output graphics reference keyword
  gRef = HASH()
  tRef = HASH()
  
  ; ...Plotting
  font  = KEYWORD_SET(ps) ? 1 : -1
  thick = KEYWORD_SET(ps) ? 2 :  1
  
;*   1) Set array of colors based on n_detectors
;*   color = [........]
  
;*   Parameters
;*   2) Set hash information include n_detectors 
;*      for sensor_id's relevant to <sensor>
;*   <sensor>_INFO = HASH (  ........... )
  
  HISTORY = "$Id:$"
  COMMENT_FILE = Path+PATH_SEP()+'source.comment'
  
  ; Get sensor information
  get_sensor_information, SensorInfo_File  , $ ; Input
                          Sensor_Id        , $ ; Input
                          Sensor_Channel   , $ ; Output
                          Sensor_Type      , $ ; Output
                          WMO_Satellite_Id , $ ; Output
                          WMO_Sensor_Id    , $ ; Output
                          n_Channels
                          
  ; Get comment
  get_comment, COMMENT_FILE, $ ; Input
               comment         ; Ouput
               
  ; Extract the sensor and platform from the sensor id
  ; to allow input filenames to be created
  sensor_id_array = STRSPLIT(Sensor_Id,'_',/EXTRACT)
  sensor   = sensor_id_array[0]
  platform = sensor_id_array[1]
  FOR i = 0, n_detectors DO BEGIN
    IF ( i LT n_detectors ) THEN BEGIN
      ; The detector file objects
      detector_sensor_id[i] = sensor+'D'+STRING(detector[i],FORMAT='(i2.2)')+'_'+platform
      title = detector_sensor_id[i]+" Spectral Response Funtions"
      osrf_file[i] = OBJ_NEW( 'OSRF_FILE', $
                              Path+PATH_SEP()+detector_sensor_id[i]+'.osrf.nc', $
                              Debug            = Debug                , $
                              Version          = Version              , $
                              Sensor_Id        = detector_sensor_id[i], $
                              WMO_Satellite_Id = wmo_satellite_id     , $
                              WMO_Sensor_Id    = wmo_sensor_id        , $
                              Sensor_Type      = sensor_type          , $
                              Title            = title                , $
                              Comment          = comment                )
    ENDIF ELSE BEGIN
      ; The average file object
      title = "Detector Averaged Spectral Response Funtions"
      osrf_file[i] = OBJ_NEW( 'OSRF_FILE', $
                              Path+PATH_SEP()+Sensor_Id+'.osrf.nc', $
                              Debug            = Debug            , $
                              Version          = Version          , $
                              Sensor_Id        = Sensor_Id        , $
                              WMO_Satellite_Id = wmo_satellite_id , $
                              WMO_Sensor_Id    = wmo_sensor_id    , $
                              Sensor_Type      = sensor_type      , $
                              Title            = title            , $
                              Comment          = comment            )
    ENDELSE
  ENDFOR
  
  ; Begin channel loop
  FOR l = 0, n_channels - 1 DO BEGIN
    PRINT, FORMAT='(4x,"Processing channel: ",i4)', sensor_channel[l]

    rtHash = HASH()
    isrf = OBJARR(n_detectors)
    
    ; Begin detector loop
    FOR i = 0, n_detectors-1 DO BEGIN
      PRINT, FORMAT='(6x,"Processing detector ",i4)', detector[i]


      ; Create new oSRF objects for this detector
      osrf    = OBJ_NEW('oSRF',Debug=Debug)
      isrf[i] = OBJ_NEW('oSRF',Debug=Debug)
            
      
      ; Load SRF data into the object
      osrf->Load_<sensor>, $
        detector_sensor_id[i], $
        sensor_channel[l], $
        Debug = Debug, $
        Path = Path, $
        History = Load_History
        
        
      ; Set the other sensor id properties
      osrf->Set_Property, $
        Debug = Debug, $
        Version          = Version, $
        Sensor_Type      = sensor_type, $
        WMO_Satellite_Id = wmo_satellite_id , $
        WMO_Sensor_Id    = wmo_sensor_id    
        
      ; Apply a response threshold cutoff if requested
      IF ( Apply_Threshold ) THEN BEGIN
        osrf->Apply_Response_Threshold, $
          Response_Threshold, $
          No_Plot=No_Threshold_Plot, $
          No_Pause=No_Pause, $
          gRef=tmpRef, $
          Debug=Debug
        rtHash[detector[i]] = tmpRef
        IF (detector[i] EQ detector[-1]) THEN tRef[sensor_channel[l]] = rtHash[*]
      ENDIF  
          
      ; Interpolate the data to a regular frequency grid
      ; ...Linearly interpolate visible channels
      IF ( sensor_type EQ VISIBLE_SENSOR ) THEN $
        osrf->Set_Flag, Debug=Debug, /Interpolation_Method
      ; ...Compute frequency grid and interpolate
      osrf->Compute_Interpolation_Frequency, $
        isrf[i], $
        /LoRes, $
        Debug=Debug
      osrf->Interpolate, $
        isrf[i], $
        Debug=Debug
        
      ; Zero any negative values in the interpolated result
      IF ( Zero_Negative ) THEN $
        isrf[i]->Zero_Negative, /No_Recalculate, Debug = Debug
                
      ; Process the data
      isrf[i]->Integrate, Debug = Debug
      isrf[i]->Compute_Central_Frequency, Debug = Debug
      isrf[i]->Compute_Planck_Coefficients, Debug = Debug
      isrf[i]->Compute_Polychromatic_Coefficients, Debug = Debug

      ; Add the interpolated oSRF to the file container
      osrf_file[i]->Add, isrf[i], Debug=Debug
   
      ; Plot the data detector by detector for inspection
      IF ( Plot_Data ) THEN BEGIN
        osrf->Get_Property, Frequency=f , Response=r
        isrf[i]->Get_Property, Frequency=fi, Response=ri
        IF ( i EQ 0 ) THEN BEGIN
          plot_name = Sensor_Id + STRING(sensor_channel[l],FORMAT='("-",i2.2)')
          p = PLOT( f, r, $
                    NAME=plot_name, $
                    TITLE=Sensor_Id+' ch.'+STRTRIM(sensor_channel[l],2), $
                    XTITLE='Frequency (cm!U-1!N)', $
                    YTITLE='Relative Response', $
                    XTICKFONT_SIZE=10, $
                    YTICKFONT_SIZE=10, $
                    /NODATA)
          p.Refresh, /DISABLE
        ENDIF
        !NULL = PLOT( f, r, $
                      OVERPLOT=p, $
                      COLOR=color[i], $
                      LINESTYLE='none', $
                      SYMBOL='diamond', $
                      SYM_SIZE=0.6)
        !NULL = PLOT( fi, ri, $
                      OVERPLOT=p, $
                      NAME='Det.'+STRING(detector[i],FORMAT='(i2.2)'), $
                      COLOR=color[i])
      ENDIF

    ENDFOR ; Detector loop
    
    ; Average the detectors
    avgsrf = OBJ_NEW('oSRF',Debug=Debug)
    avgsrf->Set_Property, $
      Debug = Debug, $
      Version    = Version          , $
      Sensor_Id  = Sensor_Id        , $
      Sensor_Type      = sensor_type      , $
      Channel          = sensor_channel[l], $
      WMO_Satellite_Id = wmo_satellite_id , $
      WMO_Sensor_Id    = wmo_sensor_id
    ; ...Perform the averaging
    avgsrf->Average, isrf, Debug=Debug
    ; ...Add the averaged oSRF to its file container
    osrf_file[n_detectors]->Add, avgsrf, Debug=Debug
    ; ...Plot the average with the detectors
    IF ( Plot_Data ) THEN BEGIN
      avgsrf->Get_Property, Frequency=fa, Response=ra
      !NULL = PLOT( fa, ra, $
                    OVERPLOT = p, $
                    COLOR='black', $
                    NAME='Average', $
                    THICK=2)
    ENDIF 
    
    ; Refresh and view plots
    IF ( Plot_Data ) THEN BEGIN
      p.Refresh
      ; ...Only pause if not at last channel
      IF ( Plot_Pause AND (sensor_channel[l] NE sensor_channel[-1]) ) THEN BEGIN
        PRINT, FORMAT='(/5x,"Press <ENTER> to continue, Q to quit")'
        q = GET_KBRD(1)
        IF ( STRUPCASE(q) EQ 'Q' ) THEN GOTO, Done
      ENDIF
    ENDIF
    
    ; Save the graphics reference
    gRef[sensor_channel[l]] = p
        
    ; Interim clean up
    OBJ_DESTROY, osrf
    
  ENDFOR  ; Channel loop
  
    ; Write the interpolated SRFs
  FOR i = 0, n_detectors DO BEGIN
    osrf_file[i]->Set_Property, Debug=Debug, $
      History = HISTORY + '; ' + Load_History
    osrf_file[i]->Write, Debug=Debug
  ENDFOR

    
  ; Cleanup
  Done:
  OBJ_DESTROY, [isrf, osrf_file], Debug = Debug

END
