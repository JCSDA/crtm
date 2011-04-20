; docformat = 'rst'

;+
; Returns the IDL version number as a string or a boolean indicating whether
; a required version is met. 
;
; :Examples:
;    Try the main-level program at the end of this file::
;
;       IDL> .run mg_idlversion
;
; :Returns:
;    string version number or boolean
;
; :Keywords:
;    require : in, optional, type=string
;       IDL version required; if set, VIS_IDLVERSION returns a boolean of 
;       whether the version requirement is met
;-
function mg_idlversion, require=require
  compile_opt strictarr

  version = (strsplit(!version.release, /extract))[0]
  
  if (n_elements(require) gt 0L) then begin  
    return, mg_cmp_version(version, require) ge 0B
  endif else begin
    return, version
  endelse
end

; main-level example

print, mg_idlversion(), format='(%"Version: %s")'
print, mg_idlversion(require='6.4') ? 'yes' : 'no', $
       format='(%"Can require 6.4: %s")'
print, mg_idlversion(require='7.1') ? 'yes' : 'no', $
       format='(%"Can require 7.1: %s")'
print, mg_idlversion(require='7.2') ? 'yes' : 'no', $
       format='(%"Can require 7.2: %s")'

end