;+
; Returns a dumpfile fileid
function open_dumpfile, file, for_output=for_output
;-
  get_lun, fileid
  if ( keyword_set(for_output) ) then $
    openw, fileid, file, /f77_unformatted $
  else $
    openr, fileid, file, /f77_unformatted
  return, fileid
end
