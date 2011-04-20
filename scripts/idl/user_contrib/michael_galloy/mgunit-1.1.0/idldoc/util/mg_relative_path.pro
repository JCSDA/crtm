; docformat = 'rst'

;+
; Create a relative path from src to dst.
;
; :Params:
;    src : in, required, type=string
;       relative or absolute path to directory to begin in
;    dst : in, required, type=string
;       relative or absolute path to describe
;
; :Keywords:
;    web : in, optional, type=boolean
;       set to indicate to create a web path (uses / instead of the platform
;       path separator)
;
; :Examples:
;    For example::
;
;       IDL> src = filepath('', subdir=['examples', 'data'])
;       IDL> print, src
;       /Applications/itt/idl70/examples/data/
;       IDL> dst = filepath('idl', subdir=['bin'])
;       IDL> print, dst
;       /Applications/itt/idl70/bin/idl
;       IDL> print, mg_relative_path(src, dst)
;       ../../bin/idl
;-
function mg_relative_path, src, dst, web=web
  compile_opt strictarr, hidden
  on_error, 2
  
  ; the web always uses /
  sep = keyword_set(web) ? '/' : path_sep()
  
  ; get absolute path of src and dst
  srcAbs = file_search(src, /fully_qualify_path, /mark_directory, count=nsrc)
  dstAbs = file_search(dst, /fully_qualify_path, /mark_directory, count=ndst)
  
  ; src and dst need to specify a single unique directory or file
  if (nsrc ne 1) then message, 'src not found or not unique'
  if (ndst ne 1) then message, 'dst not found or not unique'  
  
  ; compare src and dst to find unique parts
  srcTokens = strsplit(srcAbs, path_sep(), /extract, count=nSrcTokens)
  dstTokens = strsplit(dstAbs, path_sep(), /extract, count=nDstTokens)
  nCommonTokens = 0
  while (srcTokens[nCommonTokens] eq dstTokens[nCommonTokens]) do nCommonTokens++
  
  ; add appropriate number of ..'s (corresponding to unique part of src)
  relPath = ''
  nDotDot = nSrcTokens- nCommonTokens
  relPath += nDotDot eq 0 ? '' : strjoin(strarr(nDotDot) + '..' + sep)
  
  ; add unique part of dst
  relPath += nDstTokens - nCommonTokens eq 0 $
               ? '' $
               : strjoin(dstTokens[nCommonTokens:nDstTokens - 1L], sep)
  
  return, relPath
end