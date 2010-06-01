FUNCTION HDF_SD_VARDIR, HDFID

;- Check arguments
if (n_params() ne 1) then $
  message, 'Usage: RESULT = HDF_SD_VARDIR(HDFID)'
if (n_elements(hdfid) eq 0) then $
  message, 'HDFID is undefined'

;- Set default return value
varnames = ''

;- Get file information
hdf_sd_fileinfo, hdfid, nvars, ngatts

;- If variables were found, get variable names
if (nvars gt 0) then begin
  varnames = strarr(nvars)
  for index = 0L, nvars - 1L do begin
    varid = hdf_sd_select(hdfid, index)
    hdf_sd_getinfo, varid, name=name
    hdf_sd_endaccess, varid
    varnames[index] = name
  endfor
endif

;- Return the result
return, varnames

END
