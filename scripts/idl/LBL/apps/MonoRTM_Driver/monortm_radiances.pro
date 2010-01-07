;+
FUNCTION MonoRTM_Radiances, f     , $  ; Input frequencies in GHz
                            Infile     ; Input "generic" file to which the frequencies are added.
;-

  ; Setup error handler
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG
    RETURN, FAILURE
  ENDIF


  ; Check input
  ; ...Arguments are mandatory
  n_Frequencies = N_ELEMENTS(f)
  IF ( n_Frequencies EQ 0 ) THEN $
    MESSAGE, 'Must supply frequencies', /NONAME, /NOPRINT
  IF ( NOT Valid_String(Infile) ) THEN $
    MESSAGE, 'Must supply generic input filename', /NONAME, /NOPRINT
  ; ...Check the file exists
  fInfo = FILE_INFO(Infile)
  IF ( NOT fInfo.EXISTS ) THEN $
    MESSAGE, Infile+' not found', /NONAME, /NOPRINT
    
    
  ; Set MONORTM default files
  MONORTM_INFILE  = 'MONORTM.IN'
  MONORTM_OUTFILE = 'MONORTM.OUT'
  MONORTM_SPECTRALFILE = 'spectral_lines.dat'
  FILE_WIDTH = 200

  
  ; Convert input frequencies in GHz to inverse
  ; centimeters
  v = GHz_to_inverse_cm(f)
  
  
  ; Read the input file contents
  n_Lines = FILE_LINES(Infile)
  Generic_Input = STRARR(n_Lines)
  OPENR, lun, Infile, /GET_LUN, WIDTH = FILE_WIDTH
  READF, lun, Generic_Input
  FREE_LUN, lun


  ; Write the MONORTM.IN file
  OPENW, lun, MONORTM_INFILE, /GET_LUN, WIDTH = FILE_WIDTH
  ; ...Write the first three header lines
  PRINTF, lun, Generic_Input[0:2], FORMAT='(a)'
  ; ...Write the specific frequencies to compute
  PRINTF, lun, n_Frequencies, FORMAT='(i8)'
  PRINTF, lun, v, FORMAT='(e19.7)'
  ; ...Write the remainder of the initial input
  PRINTF, lun, Generic_Input[3:*], FORMAT='(a)'
  FREE_LUN, lun
  

  ; Run MonoRTM
  ; ...Check that spectral line file is present
  IF ( NOT (FILE_INFO(MONORTM_SPECTRALFILE)).EXISTS ) THEN $
    MESSAGE, MONORTM_SPECTRALFILE+' not found', /NONAME, /NOPRINT
  ; ...Spawn the executable
  SPAWN, 'monortm_v4.0_dbl', Std_Out, Std_Err, EXIT_STATUS = Exit_Status
  IF ( Exit_Status NE 0 ) THEN MESSAGE, 'Error running MonoRTM:'+Std_Err, /NONAME, /NOPRINT

  
  ; Read the MonoRTM output file
  OPENR, lun, MONORTM_OUTFILE, /GET_LUN, WIDTH = FILE_WIDTH
  header = STRARR(4)
  READF, lun, header
  data=DBLARR(21, N_ELEMENTS(f))
  READF, lun, data
  FREE_LUN, lun
  
  
  ; Reformat output
  ; ...Radiance is index #3
  radiance=REFORM(data[3,*])
  ; ...Put units into mW/(m2.sr.cm-1)
  radiance = radiance * 1.0d+07
  RETURN, radiance

END
