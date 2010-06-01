PRO HDF_SD_VARREAD, HDFID, VARNAME, DATA, $
  _EXTRA=EXTRA_KEYWORDS

;- Check arguments
if (n_params() ne 3) then $
  message, 'Usage: HDF_SD_VARREAD, HDFID, VARNAME, DATA'
if (n_elements(hdfid) eq 0) then $
  message, 'Argument HDFID is undefined'
if (n_elements(varname) eq 0) then $
  message, 'Argument VARNAME is undefined'
if (arg_present(data) eq 0) then $
  message, 'Argument DATA cannot be modified'

;- Get index of the requested variable
index = hdf_sd_nametoindex(hdfid, varname)
if (index lt 0) then $
  message, 'SDS was not found: ' + varname

;- Select and read the SDS
varid = hdf_sd_select(hdfid, index)
hdf_sd_getdata, varid, data, _extra=extra_keywords
hdf_sd_endaccess, varid

END
