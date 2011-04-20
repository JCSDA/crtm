; docformat = 'rst'

;+
; Returns IDLdoc version. This file is automatically edited by the build 
; process to edit the contents of the version and revision variables below.
;
; :Returns: 
;    string
;
; :Keywords: 
;    full : in, optional, type=boolean
;       set to return Subversion revision as well
;-
function idldoc_version, full=full
  compile_opt strictarr, hidden
  
  version = '3.4alpha'
  revision = '-r752'
  
  return, version + (keyword_set(full) ? (' ' + revision) : '')
end
