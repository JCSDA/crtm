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
    IF ( N_ELEMENTS(lun) GT 0 ) THEN FREE_LUN, lun
    RETURN, FAILURE
  ENDIF


  ; Check input
  ; ...Arguments are mandatory
  n_Frequencies = N_ELEMENTS(f)
  IF ( n_Frequencies EQ 0 ) THEN $
    MESSAGE, 'Must supply frequencies', /NONAME, /NOPRINT
  IF ( ~ Valid_String(Infile) ) THEN $
    MESSAGE, 'Must supply generic input filename', /NONAME, /NOPRINT
  ; ...Check the file exists
  fInfo = FILE_INFO(Infile)
  IF ( ~ fInfo.EXISTS ) THEN $
    MESSAGE, Infile+' not found', /NONAME, /NOPRINT
    
    
  ; Set MONORTM default files
  MONORTM_INFILE  = 'MONORTM.IN'
  MONORTM_OUTFILE = 'MONORTM.OUT'
  MONORTM_SPECTRALFILE = 'TAPE3'
  
  ; Other parameters
  FILE_WIDTH        = 200      ; IDL default for ASCII is only 80 or so.
  N_OUTFILE_HEADER  = 4        ; Output file has 4 lines of header info
  N_OUTFILE_COLUMNS = 22       ; Output file has 22 columns
  IDX_RADIANCE      = 4        ; The radiance data is index 4 (column 5)
  WpCM2_TO_mWpM2    = 1.0d+07  ; Conversion factor for W/(cm2.sr.cm-1) to mW/(m2.sr.cm-1)
  
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
  IF ( ~ (FILE_INFO(MONORTM_SPECTRALFILE)).EXISTS ) THEN $
    MESSAGE, MONORTM_SPECTRALFILE+' not found', /NONAME, /NOPRINT
  ; ...Spawn the executable
  SPAWN, 'monortm', Std_Out, Std_Err, EXIT_STATUS = Exit_Status
  IF ( Exit_Status NE 0 ) THEN MESSAGE, 'Error running MonoRTM:'+Std_Err, /NONAME, /NOPRINT

  
  ; Read the MonoRTM output file
  OPENR, lun, MONORTM_OUTFILE, /GET_LUN, WIDTH = FILE_WIDTH
  header = STRARR(N_OUTFILE_HEADER)
  READF, lun, header
  data=DBLARR(N_OUTFILE_COLUMNS, N_ELEMENTS(f))
  READF, lun, data
  FREE_LUN, lun
  
  
  ; Reformat output
  radiance=REFORM(data[IDX_RADIANCE,*])
  radiance = radiance * WpCM2_TO_mWpM2  ; Put units into mW/(m2.sr.cm-1)
  RETURN, radiance

END
