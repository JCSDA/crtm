;+
; Utility to convert MonoRTM ASCII output to simple format spectrum structure
;
; spectrum == {n, frequency, radiance, transmittance}
;
PRO MonoRTM_to_Spectrum, filename, $  ; Input (MonoRTM output)
                         spectrum     ; Output (structure)
;-

  x = ddread(filename,TYPE=5)
  f = REFORM(x[1,*])
  r = REFORM(x[3,*]) * 1.0d+07
  t = REFORM(x[4,*])

  spectrum = {n:N_ELEMENTS(f), frequency:TEMPORARY(f), radiance:TEMPORARY(r), transmittance:TEMPORARY(t)}

END
