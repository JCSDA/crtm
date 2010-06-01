pro mas_interleave_uw, in_file, out_file

;+
;Purpose:
;   To convert a MAS UW-Intermediate format file from
;   band-interleaved-by-line format (as created by readtape5.f)
;   to a band-interleaved format that can be read quickly by getchan.pro
;   Band-interleaved means the file is formatted as
;     Band 1 all scans
;     Band 2 all scans
;     Band 3 all scans
;     .
;     .
;     Band 50 all scans
;
;Usage:
;   MAS_INTERLEAVE_UW, IN_FILE, OUT_FILE
;-

;- open input file and get number of complete scans

openr, in_lun, in_file, /get_lun
result = fstat( in_lun )
nscans = result.size / 74200L

;- open output file

openw, out_lun, out_file, /get_lun

;- read data in blocks and re-order

blocksize = 100L
nblocks = nscans / blocksize
data = intarr( 742, 50, blocksize )

for block = 1L, nblocks do begin
  readu, in_lun, data
  for band = 1L, 50L do begin
    offset = ( band - 1 ) * 2L * 742L * nscans + $
      ( block - 1 ) * 2L * 742L * blocksize
    point_lun, out_lun, offset
    writeu, out_lun, data( *, band - 1, * )
  endfor
endfor

;- read any scans that are left over

nleft = nscans mod blocksize
if nleft eq 0 then goto, finish
data = intarr( 742, 50, nleft )
readu, in_lun, data
for band = 1L, 50L do begin
  offset = ( band - 1 ) * 2L * 742L * nscans + $
    nblocks * 2L * 742L * blocksize
  point_lun, out_lun, offset
  writeu, out_lun, data( *, band - 1, * )
endfor

;- finish up

finish:
free_lun, in_lun
free_lun, out_lun

end
