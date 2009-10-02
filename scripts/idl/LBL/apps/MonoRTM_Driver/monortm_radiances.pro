;+
FUNCTION MonoRTM_Radiances, f  ; Input frequencies in GHz
  
;-
  ; set MONORTM default files
  INFILE='MONORTM.IN'
  OUTFILE='MONORTM.OUT'
  
  ; convert frequency in GHz to inverse
  ; centimeters
  v=ghz_to_inverse_cm(f)
  
  ; get number of lines in file
  ; and set generic string array size
  ; accordingly
  generic_lines=FILE_LINES(INFILE)
  TAPE5_Generic=STRARR(generic_lines)
  n_pts=N_ELEMENTS(f)
  
  ; Read the generic file contents
  ; into a string array
  openr, lun, INFILE, /GET_LUN, WIDTH=200
  readf, lun, TAPE5_Generic
  close, /all

  ; Write the MONORTM.IN file
  openw, lun, INFILE, /GET_LUN
  printf, lun, TAPE5_Generic[0:2]
  printf, lun, n_pts, format='(i8)'
  printf, lun, v, format='(e19.7)'
  printf, lun, TAPE5_Generic[3:generic_lines-1], format='(a)'
  close, /all
  
  ; Get radiances for the relative response frequencies
  SPAWN,'monortm_v4.0_dbl'
  
  ; set MONORTM.IN back to its generic form
  openw, lun, INFILE, /GET_LUN
  printf, lun, TAPE5_Generic[0:17], format='(a)'
  printf, lun, TAPE5_Generic[18], format='(a)'
  printf, lun, TAPE5_Generic[19:generic_lines-1], format='(a)'
  close, /all

  OPENR, lun, OUTFILE, /GET_LUN
    header=STRARR(4)
    READF, lun, header
    data=DBLARR(21, N_ELEMENTS(f))
    READF, lun, data
  Free_lun, lun
  
  radiance=REFORM(data[3,*])
  radiance=radiance*double(1E7)
  return, radiance

END
