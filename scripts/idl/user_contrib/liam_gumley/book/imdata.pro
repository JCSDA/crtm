FUNCTION IMDATA, FILE, NX, NY

;- Load an example image
openr, lun, filepath(file, subdir='examples/data'), /get_lun
image = bytarr(nx, ny)
readu, lun, image
free_lun, lun
return, image

END
