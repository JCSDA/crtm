; docformat = 'rst'

;+
; Verbatim parsing just makes everything text and inserts the end-of-line 
; nodes.
;-


;+
; Takes a string array of free text comments and return a parse tree.
;
; :Abstract:
; 
; :Returns: 
;    object
;
; :Params:
;    lines : in, required, type=strarr
;       lines to be parsed
;
; :Keywords:
;    top : in, optional, type=string, default=paragraph
;       type of top-level MGtmTag to create
;-
function docparpreformattedmarkupparser::parse, lines, top=top, file=file
  compile_opt strictarr, hidden
  
  tree = obj_new('MGtmTag', type=n_elements(top) gt 0 ? top : 'preformatted')
  
  for l = 0L, n_elements(lines) - 1L do begin
    tree->addChild, obj_new('MGtmText', text=lines[l])
    tree->addChild, obj_new('MGtmTag', type='newline')
  endfor
  
  return, tree
end


;+
; Define instance variables.
;-
pro docparpreformattedmarkupparser__define
  compile_opt strictarr, hidden
  
  define = { DOCparPreformattedMarkupParser, inherits DOCparMarkupParser }
end