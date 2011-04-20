; docformat = 'rst'

;+
; Filters a line to make it safe for LaTeX output, i.e., escaping certain
; characters with backslashes.
;
; :Returns:
;    string
;
; :Params:
;    line : in, required, type=string
;       line to filter
;-
function mg_escape_latex, line
  compile_opt strictarr
  
  _line = mg_streplace(line, '_', '\_', /global)
  _line = mg_streplace(_line, '\$', '\$', /global)
  _line = mg_streplace(_line, '{', '\{', /global)
  _line = mg_streplace(_line, '}', '\}', /global)  
  
  return, _line
end