FUNCTION READ_WEATHER, FILE

;- Check arguments
if (n_elements(file) eq 0) then $
  message, 'Argument FILE is undefined'

;- Open the input file
openr, lun, file, /get_lun

;- Define record structure, and create data array
record = {id:0L, year:0, month:0, day:0, hour:0, minute:0, $
  pres:0.0, temp:0.0, dewp:0.0, speed:0.0, dir:0.0}
data = replicate(record, 24L * 60L)

;- Read records from the file until EOF
nrecords = 0
while (eof(lun) ne 1) do begin
  readu, lun, record
  data[nrecords] = record
  nrecords = nrecords + 1
endwhile

;- Close the input file
free_lun, lun

;- Trim the data array, and return it to caller
data = data[0 : nrecords - 1]
return, data

END
