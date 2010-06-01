PRO REDIRECT, OUTFILE=OUTFILE

;- Select logical unit for output
if (n_elements(outfile) eq 1) then begin
  openw, outlun, outfile, /get_lun
endif else begin
  outlun = -1
endelse

;- printf is used in the program for output
printf, outlun, 'Starting program execution at ', systime()

;- Body of program goes here

;- Close the output file if needed
if (outlun gt 0) then free_lun, outlun

END
