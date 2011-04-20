; docformat = 'rst'

;+
; Returns the drive letter (on Windows) or the empty string (otherwise) of the 
; given path. Returns the empty string on Windows for UNC paths like::
;
;    \\host\share\directory\file.
;
; :Returns: string
; :Params:
;    path : in, required, type=string
;       relative or absolute path
;-
function mg_getdrive, path
  compile_opt strictarr, hidden
  on_error, 2
  
  if (strlowcase(!version.os_family) ne 'windows') then return, ''
  
  pathAbs = file_search(path, /fully_qualify_path, /mark_directory, count=nPath)
  if (nPath ne 1) then message, 'path not found or not unique'
  
  return, strmid(pathAbs, 0, 1)
end