PRO BINREAD, LUN, DATA

;- Check arguments
if (n_elements(lun) eq 0) then $
  message, 'LUN is undefined'
if (arg_present(data) eq 0) then $
  message, 'DATA cannot be modified'

;- Check that file is open
result = fstat(lun)
if (result.open eq 0) then $
  message, 'LUN does not point to an open file'

;- Read magic value
magic_value = 0L
readu, lun, magic_value

;- Decode magic value to see if byte swapping is required
swap_flag = 0
if (magic_value ne 123456789L) then begin
  magic_value = swap_endian(magic_value)
  if (magic_value eq 123456789L) then begin
    swap_flag = 1
  endif else begin
    message, 'File was not written with BINWRITE'
  endelse
endif

;- Read the header, swapping if necessary
info_size = 0L
readu, lun, info_size
if swap_flag then info_size = swap_endian(info_size)
info = lonarr(info_size)
readu, lun, info
if swap_flag then info = swap_endian(info)

;- Read the data, swapping if necessary
data = make_array(size=info)
readu, lun, data
if swap_flag then data = swap_endian(temporary(data))

END
