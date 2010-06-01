PRO BINWRITE, LUN, DATA

;- Check arguments
if (n_elements(lun) eq 0) then $
  message, 'LUN is undefined'
if (n_elements(data) eq 0) then $
  message, 'DATA is undefined'

;- Check that file is open
result = fstat(lun)
if (result.open eq 0) then $
  message, 'LUN does not point to an open file'

;- Check that data type is allowed
type_name = size(data, /tname)
if (type_name eq 'STRING') or $
   (type_name eq 'STRUCT') or $
   (type_name eq 'POINTER') or $
   (type_name eq 'OBJREF') then $
   message, 'DATA type is not supported'

;- Check that DATA is an array
if (size(data, /n_dimensions) lt 1) then $
  message, 'DATA must be an array'

;- Create header array
magic_value = 123456789L
info = size(data)
info_size = n_elements(info)
header = [magic_value, info_size, info]

;- Write header and data to file
writeu, lun, header
writeu, lun, data

END
