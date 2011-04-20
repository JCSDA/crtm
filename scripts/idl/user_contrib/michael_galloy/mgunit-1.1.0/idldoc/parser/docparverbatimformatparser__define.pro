; docformat = 'rst'

;+
; Handles parsing of comment blocks by just passing comments along.
;-


;+
; Handles parsing of a routine level comment block with no special syntax: all 
; comments are passed through to the markup parser. 
;
; :Params:
;    lines : in, required, type=strarr
;       all lines of the comment block
;
; :Keywords:
;    routine : in, required, type=object
;       routine tree object 
;    markup_parser : in, required, type=object
;       markup parser object
;-
pro docparverbatimformatparser::parseRoutineComments, lines, routine=routine, $
                                                      markup_parser=markupParser
  compile_opt strictarr, hidden
  
  comments = markupParser->parse(lines)
  routine->setProperty, comments=comments
end


;+
; Handles parsing of a file level comment block with no special syntax: all 
; comments are passed through to the markup parser. 
;
; :Params:
;    lines : in, required, type=strarr
;       all lines of the comment block
;
; :Keywords:
;    file : in, required, type=object
;       file tree object 
;    markup_parser : in, required, type=object
;       markup parser object
;-
pro docparverbatimformatparser::parseFileComments, lines, file=file, $
                                                   markup_parser=markupParser
  compile_opt strictarr, hidden
  
  comments = markupParser->parse(lines)
  file->setProperty, comments=comments
end


;+
; Handles parsing of the directory overview comment block with no special 
; syntax: all comments are passed through to the markup parser. 
;
; :Params:
;    lines : in, required, type=strarr
;       all lines of the comment block
;
; :Keywords:
;    directory : in, required, type=object
;       directory object 
;    markup_parser : in, required, type=object
;       markup parser object
;-
pro docparverbatimformatparser::parseDirOverviewComments, lines, $
                                                          directory=directory, $
                                                          markup_parser=markupParser
  compile_opt strictarr, hidden
  
  comments = markupParser->parse(lines)
  directory->setProperty, comments=comments
end


;+
; Handles parsing of the overview comment block with no special syntax: all 
; comments are passed through to the markup parser. 
;
; :Params:
;    lines : in, required, type=strarr
;       all lines of the comment block
;
; :Keywords:
;    system : in, required, type=object
;       system object 
;    markup_parser : in, required, type=object
;       markup parser object
;-
pro docparverbatimformatparser::parseOverviewComments, lines, system=system, $
                                                       markup_parser=markupParser
  compile_opt strictarr, hidden
  
  comments = markupParser->parse(lines)
  system->setProperty, overview_comments=comments
end


;+
; Define instance variables.
;-
pro docparverbatimformatparser__define
  compile_opt strictarr, hidden

  define = { docparverbatimformatparser, inherits DOCparFormatParser }
end