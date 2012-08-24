;+
; Utility to write simple format spectrum datafiles
;
; spectrum == {n, frequency, radiance, transmittance}
;
PRO Spectrum_WriteFile, filename, $  ; Input (if exists, is overwritten)
                        spectrum     ; Input
;-

  OPENW, fileid, filename, /GET_LUN
  WRITEU, fileid, spectrum.n
  WRITEU, fileid, spectrum.frequency
  WRITEU, fileid, spectrum.radiance
  WRITEU, fileid, spectrum.transmittance
  FREE_LUN, fileid
  
END
