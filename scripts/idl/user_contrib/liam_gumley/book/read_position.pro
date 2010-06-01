FUNCTION READ_POSITION, FILE, MAXREC=MAXREC

;- Check arguments
if (n_elements(file) eq 0) then $
  message, 'Argument FILE is undefined'
if (n_elements(maxrec) eq 0) then $
  maxrec = 10000L

;- Open input file
openr, lun, file, /get_lun

;- Define record structure and create array
fmt = '(2(i2, 1x), f5.2, 4x, f8.4, 2x, f9.4, 2x, f5.1)'
record = {hour:0L, min:0L, sec:0.0, $
  lat:0.0, lon:0.0, head:0.0}
data = replicate(record, maxrec)

;- Read records until end-of-file reached
nrecords = 0L
recnum = 1L
while (eof(lun) ne 1) do begin

  ;- Read this record (jumps to bad_rec: on error)
  on_ioerror, bad_rec
  error = 1
  readf, lun, record, format=fmt
  error = 0

  ;- Store data for this record
  data[nrecords] = record
  nrecords = nrecords + 1L

;- Check if maximum record count exceeded
  if (nrecords eq maxrec) then begin
    free_lun, lun
    message, 'Maximum record reached: increase MAXREC'
  endif

  ;- Check for bad input record
  bad_rec:
  if (error eq 1) then $
    print, 'Bad data at record ', recnum
  recnum = recnum + 1

endwhile

;- Close input file
free_lun, lun

;- Trim data array and return it to caller
data = data[0 : nrecords - 1]
return, data

END
