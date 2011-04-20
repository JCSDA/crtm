; docformat = 'rst'

;+
; Returns the platform extension used by the PLATFORM_EXTENSION keyword to
; MAKE_DLL.
;
; :Returns:
;    string
;
; :Keywords:
;    extension : in, optional, type=boolean
;       append appropriate shared object extension
;-
function mg_platform_extension, extension=extension
  compile_opt strictarr
  
  ext = !version.os_family eq 'unix' ? '.so' : '.dll'
  return, strmid(expand_path('<IDL_BIN_DIRNAME>'), 4) $
            + (keyword_set(extension) ? ext : '')
end


; main-level example program

print, mg_platform_extension(), format='(%"Platform extension: %s")'

end
