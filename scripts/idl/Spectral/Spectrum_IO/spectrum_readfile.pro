;+
; Utility to read simple format spectrum datafiles
;
; spectrum == {n, frequency, radiance, transmittance}
;
PRO Spectrum_ReadFile, filename, $  ; Input (must exist)
                       spectrum     ; Output
;-

  OPENR, fileid, filename, /GET_LUN
  n = 0L
  READU, fileid, n
  f = DBLARR(n)
  r = DBLARR(n)
  t = DBLARR(n)
  READU, fileid, f
  READU, fileid, r
  READU, fileid, t
  FREE_LUN, fileid
  
  spectrum = {n:n, frequency:TEMPORARY(f), radiance:TEMPORARY(r), transmittance:TEMPORARY(t)}
  
END
