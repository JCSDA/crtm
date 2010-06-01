FUNCTION HDF_SD_ATTDIR, HDFID, VARNAME

;- Check arguments
if (n_params() ne 2) then $
  message, 'Usage: RESULT = HDF_SD_ATTDIR(HDFID, VARNAME)'
if (n_elements(hdfid) eq 0) then $
  message, 'HDFID is undefined'
if (n_elements(varname) eq 0) then $
  message, 'VARNAME is undefined'

;- Set default return value
attnames = ''

;- Get attribute information
if (varname eq '') then begin
  hdf_sd_fileinfo, hdfid, nvars, natts
endif else begin
  index = hdf_sd_nametoindex(hdfid, varname)
  varid = hdf_sd_select(hdfid, index)
  hdf_sd_getinfo, varid, natts=natts
endelse

;- If attributes were found, get attribute names
if (natts gt 0) then begin
  attnames = strarr(natts)
  for index = 0L, natts - 1L do begin
    if (varname eq '') then begin
      hdf_sd_attrinfo, hdfid, index, name=name
    endif else begin
      hdf_sd_attrinfo, varid, index, name=name
    endelse
    attnames[index] = name
  endfor
endif

;- End access to this variable if necessary
if (varname ne '') then hdf_sd_endaccess, varid

;- Return the result
return, attnames

END
