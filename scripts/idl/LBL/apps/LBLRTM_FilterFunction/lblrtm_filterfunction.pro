;+
FUNCTION LBLRTM_FilterFunction, $
  f1, f2  , $  ; Input. The LBLRTM begin and end frequencies
  SRF_file, $  ; Input. The netCDF SRF file(s) containing the filter data.
  Debug = debug
;-

  ; Setup
  @error_codes
  ; ...Define error handler
  IF ( KEYWORD_SET(debug) ) THEN BEGIN
    MESSAGE, '--> Entered.', /INFORMATIONAL
    msgswitch = 0
  ENDIF ELSE BEGIN
    CATCH, err_stat
    IF ( err_stat NE 0 ) THEN BEGIN
      CATCH, /CANCEL
      MESSAGE, !ERROR_STATE.MSG, /CONTINUE
      RETURN, FALSE
    ENDIF
    msgswitch = 1
  ENDELSE
  ; ...Define parameters
  T5_FILE_WIDTH = 200
  FILTER_FMT = '(10f8.4)'
  SPECTRAL_FILE = 'TAPE3'


  ; Check input
  n_srf_files = N_ELEMENTS(SRF_file)
  IF ( n_srf_files LT 1 ) THEN $
    MESSAGE, 'No oSRF datafiles provided', NONAME=msgswitch, NOPRINT=msgswitch


  ; Get the generic profile data filelist
  gt5_file = FILE_SEARCH('generic_input'+PATH_SEP()+'TAPE5.*', COUNT = n_gt5_files)
  IF ( n_gt5_files LT 1 ) THEN $
    MESSAGE, 'No generic input TAPE5 files found', NONAME=msgswitch, NOPRINT=msgswitch

    
  ; Read the oSRF datafiles
  osrf_file = HASH()
  FOR n = 0, n_srf_files - 1 DO BEGIN
    osrf_file[n] = OBJ_NEW('osrf_file', SRF_file[n], Debug = debug)
    osrf_file[n].Read, Debug = debug
  ENDFOR


  ; Initialise the return variable
  lblrtm_result = HASH()
  
  
  ; Begin loop over profiles (i.e. generic TAPE5 files)
  FOR m = 0L, n_gt5_files-1L DO BEGIN
  
    PRINT, FILE_BASENAME(gt5_file[m]), FORMAT='(5x,"Processing ",a,"...")'

    ; Read the generic input file contents
    n_lines = FILE_LINES(gt5_file[m])
    gt5_input = STRARR(n_lines)
    OPENR, fid, gt5_file[m], /GET_LUN, WIDTH = T5_FILE_WIDTH
    READF, fid, gt5_input
    FREE_LUN, fid
    ; ...Copy the generic input
    t5_input = gt5_input
    ; ...Replace the frequency bound markers
    t5_input[2] = STRING(f1,f2,FORMAT='(2f10.3)')


    ; Initialise the sensor/channel identifier
    sid_ch = []


    ; Append the channel filter functions to the gt5 input
    FOR n = 0, n_srf_files - 1 DO BEGIN
      osrf_file[n].Get_Property, $
        Debug = debug, $
        Sensor_Id = sensor_id, $
        n_Channels = n_channels

      FOR l = 0, n_channels-1 DO BEGIN
        osrf = osrf_file[n].Get(Position = l, Debug = debug)
        ; Get the SRF bounds
        osrf.Get_Property, $
          f1 = sf1, f2 = sf2, $
          Channel = channel, $
          Debug = debug
        ; Cycle channel loop if out-of-bounds
        IF ( (sf1 LT f1) OR (sf2 GT f2) ) THEN CONTINUE
        ; Get SRF repsonse data
        osrf.Get_Property, $
          n_Points = n_points, $
          Response = response, $
          Debug = debug
        ; Compute the wavenumber interval
        df = (sf2 - sf1)/DOUBLE(n_points-1L)
        ; Print in LBLRTM format
        filter_header   = STRING(sf1, df, n_points, $
                                 FORMAT='(f10.3,f10.4,i5,"    1   12    1   30")')
        filter_format   = STRING(FILTER_FMT, FORMAT='(a)')
        filter_response = STRING(response, FORMAT=FILTER_FMT)
        ; Append to the TAPE5 input
        t5_input = [t5_input, filter_header, filter_format, filter_response]
        ; Define this sensor/channel combination
        sid_ch = [sid_ch, STRTRIM(sensor_id,2)+'-'+STRTRIM(channel,2)]
      ENDFOR  ; Channel loop
    ENDFOR  ; SRF file loop


    ; Output the TAPE5 input
    OPENW, fid, 'TAPE5', /GET_LUN, WIDTH = T5_FILE_WIDTH
    PRINTF, fid, t5_input, FORMAT='(a)'
    PRINTF, fid, -1.0, FORMAT='(f4.1)'
    FREE_LUN, fid


    ; Run LBLRTM
    ; ...Check that spectral line file is present
    IF ( ~ (FILE_INFO(SPECTRAL_FILE)).EXISTS ) THEN $
      MESSAGE, SPECTRAL_FILE+' not found', NONAME=msgswitch, NOPRINT=msgswitch
    ; ...Spawn the executable
    SPAWN, 'lblrtm', stdout, stderr, EXIT_STATUS = exit_stat
    IF ( exit_stat NE 0 ) THEN BEGIN
      FOREACH s, stderr DO MESSAGE, s, /CONTINUE
      MESSAGE, 'Error running LBLRTM for '+FILE_BASENAME(gt5_file[m]), /CONTINUE
      MESSAGE, '...skipping to next profile', /CONTINUE
      CONTINUE  ; Next profile
    ENDIF


    ; Parse the TAPE6 output
    ; ...Grep the file
    SPAWN, 'grep "INTEGRATED EMISSION" TAPE6', stdout, stderr, EXIT_STATUS = exit_stat
    IF ( exit_stat NE 0 ) THEN $
      MESSAGE, 'Error grep-ing TAPE6:'+stderr, NONAME=msgswitch, NOPRINT=msgswitch
    ; ...Initialise the current profile result
    profile_result = HASH()
    ; ...Extract the information, with units in mW/(m2.sr.cm-1)
    n_sid_ch = N_ELEMENTS(stdout)
    normalised_emission = 0.0d0
    FOR i = 0, n_sid_ch-1 DO BEGIN
      READS, stdout[i], normalised_emission, FORMAT='(101x,e12.5)'
      profile_result[sid_ch[i]] = normalised_emission * 1.0d+07
    ENDFOR
    

    ; And save the final result
    lblrtm_result[FILE_BASENAME(gt5_file[m])] = profile_result

  ENDFOR  ; Profile loop


  ; Done
  RETURN, lblrtm_result

END
