FUNCTION NCDF_ISNCDF, FILE

;- Check arguments
if (n_params() ne 1) then $
  message, 'Usage: RESULT = NCDF_ISNCDF(FILE)'
if (n_elements(file) eq 0) then $
  message, 'Argument FILE is undefined'

;- Establish error handler
catch, error_status
if (error_status ne 0) then return, 0

;- Attempt to open the file
ncid = ncdf_open(file)

;- Close the file and return to caller
ncdf_close, ncid
return, 1

END
