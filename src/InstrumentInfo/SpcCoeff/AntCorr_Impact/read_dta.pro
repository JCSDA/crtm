pro read_dta, file, ta, ta_ra, data_id

  openr, fileid, file, /f77_unformatted, /get_lun

  ; dimensions
  nv = 0L
  nt = 0L
  nc = 0L
  nf = 0L
  readu, fileid, nv, nt, nc, nf
  
  ; data identifiers
  sl = 0L
  readu, fileid, sl
  x = bytarr(sl,nv)
  readu, fileid, x
  data_id = strarr(nv)
  for i=0, nv-1 do $
    data_id[i] = string(x[*,i])
    
  ; dimension data
  tearth=dblarr(nt)
  sensor_channel=lonarr(nc)
  readu, fileid, tearth, sensor_channel
  
  ta = dblarr(nf, nc, nt, nv)
  ta_ra = dblarr(nf, nc, nt, nv)
  
  t1 = dblarr(nf)
  t2 = dblarr(nf)
  for k=0,nv-1 do begin
    for j=0,nt-1 do begin
      for i=0,nc-1 do begin
        readu, fileid, t1, t2
        ta[*,i,j,k]=t1
        ta_ra[*,i,j,k]=t2
      endfor
    endfor
  endfor

  free_lun, fileid
  
end
