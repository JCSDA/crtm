FUNCTION NCDF_ATTDIR, CDFID, VARNAME

;- Check arguments
if (n_params() ne 2) then $
  message, 'Usage: RESULT = NCDF_ATTDIR(CDFID, VARNAME)'
if (n_elements(cdfid) eq 0) then $
  message, 'Argument CDFID is undefined'
if (n_elements(varname) eq 0) then $
  message, 'Argument VARNAME is undefined'

;- Set default return value
attnames = ''

;- Get attribute information
if (varname eq '') then begin
  fileinfo = ncdf_inquire(cdfid)
  natts = fileinfo.ngatts
endif else begin
  varid = ncdf_varid(cdfid, varname)
  varinfo = ncdf_varinq(cdfid, varid)
  natts = varinfo.natts
endelse

;- If attributes were found, get attribute names
if (natts gt 0) then begin
  attnames = strarr(natts)
  for index = 0L, natts - 1L do begin
    if (varname eq '') then begin
      name = ncdf_attname(cdfid, index, /global)
    endif else begin
      name = ncdf_attname(cdfid, varid, index)
    endelse
    attnames[index] = name
  endfor
endif

;- Return the result
return, attnames

END
