function read_sdd, file

  if ( not valid_string(file) ) then file = 'LF_MWSSEMCoeff.bin'

  ; Open the file
  fid = open_binary_file(file)
  
  ; Read the release and version
  rel = 0L
  ver = 0L
  readu, fid, rel, ver

  ; Read the dimensions
  n_w = 0L
  n_f = 0L
  readu, fid, n_w, n_f
  
  ; Read the data
  w   = dblarr(n_W)
  f   = dblarr(n_f)
  sdd = dblarr(n_w,n_f)
  readu,fid,w
  readu,fid,f
  readu,fid,sdd
  free_lun, fid
  
  ; create return structure
  return, {w:temporary(w),f:temporary(f),sdd:temporary(sdd)}
  
end

