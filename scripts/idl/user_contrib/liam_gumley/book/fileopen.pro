FUNCTION FILEOPEN, FILENAME, ERRTEXT=ERRTEXT

;-Set default return value
result = -1

;- Check that filename is defined
if (n_elements(filename) eq 0) then begin
  errtext = 'Filename is undefined'
  return, result
endif

;- Check that file exists
if ((findfile(filename))[0] eq '') then begin
  errtext = 'Filename was not found'
  return, result
endif

;- Open the file
openr, lun, filename, /get_lun
errtext = ''
return, lun

END
