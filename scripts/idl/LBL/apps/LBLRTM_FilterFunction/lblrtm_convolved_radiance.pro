;+
FUNCTION LBLRTM_Convolved_Radiance, $
  f1, f2  , $  ; Input. The filter begin and end frequencies in cm^-1
  Response, $  ; Input. The filter response data. Evenly spaced in frequency.
  GT5_file, $  ; Input. The generic input file for LBLRTM
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
      FREE_LUN, fid
      RETURN, FALSE
    ENDIF
    msgswitch = 1
  ENDELSE
  ; ...Define parameters
  LBLRTM_INFILE       = 'TAPE5'
  LBLRTM_OUTFILE      = 'TAPE6'
  LBLRTM_SPECTRALFILE = 'TAPE3'
  FILE_WIDTH = 200
  FILTER_FMT = '(10f8.4)'
  TEN = 10.0d0
  MAX_N_RUNS = 10
  ; ...Define default return value
  radiance = 0.0d0


  ; Check input
  IF ( NOT Valid_String(GT5_file) ) THEN $
    MESSAGE, 'Must supply generic input filename', NONAME=msgswitch, NOPRINT=msgswitch
  IF ( ~ FILE_TEST(GT5_file) ) THEN $
    MESSAGE, GT5_file+' not found', NONAME=msgswitch, NOPRINT=msgswitch


  ; Check that the spectral line file is present
  IF ( ~ FILE_TEST(LBLRTM_SPECTRALFILE) ) THEN $
    MESSAGE, LBLRTM_SPECTRALFILE+' not found', NONAME=msgswitch, NOPRINT=msgswitch

    
  ; Read the generic input file contents
  n_lines = FILE_LINES(GT5_file)
  t5_input = STRARR(n_lines)
  OPENR, fid, GT5_file, /GET_LUN, WIDTH = FILE_WIDTH
  READF, fid, t5_input
  FREE_LUN, fid


  ; Replace the control card
  control=" HI=1 F4=1 CN=1 AE=0 EM=1 SC=0 FI=1 PL=0 TS=0 AM=1 M=00 LA=0 OD=0 XS=0    0    0"
  t5_input[1] = control


  ; Replace the emissivity/reflectivity
  ; ...Specify default IR emissivity/reflectivity
  surface_emissivity   = 0.95
  surface_reflectivity = 0.05
  ; ...Get the current surface temperature
  READS, t5_input[3], surface_temperature
  ; ...Replace emissivity/reflectivity
  t5_input[3] = STRING(surface_temperature,surface_emissivity,surface_reflectivity,FORMAT='(2f10.3,20x,f10.3)')

  
  ; Append the channel filter functions to the gt5 input
  n_points = N_ELEMENTS(response)
  ; ...Compute the wavenumber interval
  df = (f2 - f1)/DOUBLE(n_points-1L)
  ; ...Print in LBLRTM format
  filter_header   = STRING(f1, df, n_points, $
                           FORMAT='(f10.3,f10.4,i5,"    1   12    1   30")')
  filter_format   = STRING(FILTER_FMT, FORMAT='(a)')
  filter_response = STRING(response, FORMAT=FILTER_FMT)
  ; ...Append to the TAPE5 input
  t5_input = [t5_input, filter_header, filter_format, filter_response]


  ; Repeat frequency bound change and run based on weird LBLRTM error
  n_runs  = 0
  delta_f = 9.5d0
  REPEAT BEGIN
  
    ; Increment the frequency bound change
    delta_f = delta_f + 0.5d0
    
    
    ; Replace the frequency bound markers
    cf1 = f1 - delta_f
    cf2 = f2 + delta_f
    t5_input[2] = STRING(cf1,cf2,FORMAT='(2f10.3)')


    ; Write the TAPE5 file
    OPENW, fid, LBLRTM_INFILE, /GET_LUN, WIDTH = FILE_WIDTH
    PRINTF, fid, t5_input, FORMAT='(a)'
    PRINTF, fid, -1.0, FORMAT='(f4.1)'
    FREE_LUN, fid


    ; Run LBLRTM
    ; ...Spawn the executable
    SPAWN, 'lblrtm', stdout, stderr, EXIT_STATUS = exit_stat
    ; ...Check the result
    run_error = ( exit_stat NE 0 )
    IF ( run_error ) THEN BEGIN
      FOREACH s, stderr DO MESSAGE, s, /CONTINUE
      MESSAGE, 'Error running LBLRTM for '+FILE_BASENAME(GT5_file) + '. Trying again...', $
               /INFORMATIONAL
    ENDIF
    ; ...only keep trying so many times...
    n_runs++
  
  ENDREP UNTIL (~run_error OR n_runs GT MAX_N_RUNS)


  ; Check if screwup occurred
  IF ( run_error AND n_runs GT MAX_N_RUNS ) THEN $
    MESSAGE, 'Number of repeated LBLRTM runs exceeded maximum!', $
             NONAME=msgswitch, NOPRINT=msgswitch

             
  ; Parse the TAPE6 output
  ; ...Grep the file
  SPAWN, 'grep "INTEGRATED EMISSION" '+LBLRTM_OUTFILE, stdout, stderr, EXIT_STATUS = exit_stat
  IF ( exit_stat NE 0 ) THEN $
    MESSAGE, 'Error grep-ing '+LBLRTM_OUTFILE+':'+stderr, NONAME=msgswitch, NOPRINT=msgswitch
  ; ...Extract the information, with units in mW/(m2.sr.cm-1)
  READS, stdout[0], radiance, FORMAT='(101x,e12.5)'
  radiance = radiance * 1.0d+07
  RETURN, radiance

END
