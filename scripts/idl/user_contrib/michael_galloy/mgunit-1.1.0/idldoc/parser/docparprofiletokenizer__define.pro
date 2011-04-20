; docformat = 'rst'

;+
; This class will iterate through IDL commands (even if there are multiple
; commands on one line).
;-


;+
; Strip comments from a line of code. Returns the line of code without the 
; comments.
; 
; :Returns: 
;    string
;
; :Params:
;    line : in, required, type=string
;       line of IDL code
;
; :Keywords:
;    empty : out, optional, type=boolean
;       true if there is no IDL statement on the line (only comments or 
;       whitespace)
;-
function docparprofiletokenizer::_stripComments, line, empty=empty
  compile_opt strictarr, hidden

  semicolonPosition = strpos(line, ';')
  while (semicolonPosition ne -1L) do begin
    before = strmid(line, 0, semicolonPosition)
    
    beforeAsBytes = byte(before)
    ; ' = 39B, " = 34B
    ind = where(beforeAsBytes eq 34B or beforeAsBytes eq 39B, count)
    
    ; the semicolon is not in a string because there are no quotes
    if (count eq 0) then return, before
    
    looking = 0B   ; true if already seen a quote and looking for a match
    lookingFor = 0B   ; which quote to look for, either 39B or 34B
    
    ; loop through each ' or " before the current semicolon
    for i = 0L, n_elements(ind) - 1L do begin
      cur = beforeAsBytes[ind[i]]
      
      if (~looking) then begin
        looking = 1B
        lookingFor = cur
        continue
      endif 
      
      if (cur eq lookingFor) then looking = 0B
    endfor
    
    ; strings before semicolon are completed so return everything before 
    ; the semicolon
    if (~looking) then return, before
    
    ; semicolon is inside a string, so go to the next semicolon
    semicolonPosition = strpos(line, ';', semicolonPosition + 1L)
  endwhile 
  
  ; no comments found
  return, line
end


;+
; Returns whether there is another command.
;
; :Returns: 
;    1 if has another token, 0 if not
;-
function docparprofiletokenizer::hasNext
  compile_opt strictarr, hidden
  
  if (self.currentLineIndex lt self.nLines - 1L) then return, 1B
  if (self.currentCommandIndex lt self.nCommands) then return, 1B
  
  ; self.currentCommandIndex is at self.nCommands or beyond on the last line, 
  ; so no more tokens
  return, 0B
end


;+
; Returns the next command.
;
; :Returns: 
;    string
;
; :Keywords:
;    current_line_number : out, optional, type=long
;       returns the line number of the current (possibly partial) returned line
;-
function docparprofiletokenizer::next, current_line_number=currentLineNumber
  compile_opt strictarr, hidden
  on_error, 2
  
  ; there is a command left on the current line
  if (self.currentCommandIndex lt self.nCommands) then begin
    commands = strsplit(self.currentLine, '&', /extract)
    return, strtrim(commands[self.currentCommandIndex++], 2)
  endif 
  
  self.currentLineIndex++
  currentLineNumber = self.currentLineIndex
  
  ; no more commands left
  if (self.currentLineIndex ge self.nLines) then begin
    message, 'no more commands left in file'
  endif 
  
  ; read a new line
  self.currentCommandIndex = 0L
  self.currentLine = strtrim((*self.pLines)[self.currentLineIndex], 2)
  
  ; check to see if it's comment line
  if (strmid(self.currentLine, 0, 1) eq ';') then begin
    self.nCommands = 1L
    self.currentCommandIndex++
    return, self.currentLine
  endif
  
  ; it must have IDL commands on the line
  self.currentLine = self->_stripComments(self.currentLine)
  commands = strsplit(self.currentLine, '&', /extract, count=nCommands)
  self.nCommands = nCommands
  return, strtrim(commands[self.currentCommandIndex++], 2)
end


;+
; Free resources.
;-
pro docparprofiletokenizer::cleanup
  compile_opt strictarr, hidden
  
  ptr_free, self.pLines
end


;+
; Create a tokenizer.
;
; :Returns: 
;    1 for success, 0 for failure
;
; :Params:
;    lines : in, required, type=strarr
;       text of the .pro file
;-
function docparprofiletokenizer::init, lines
  compile_opt strictarr, hidden
  
  self.pLines = ptr_new(lines)
  self.nLines = n_elements(lines)
  self.currentLineIndex = -1L
  self.currentLine = lines[0]
  self.nCommands = 0L
  self.currentCommandIndex = 0L
  
  return, 1
end


;+
; Define instance variables.
;
; :Fields:
;    plines
;       pointer to code strarr
;    nLines
;       number of lines held by pLines pointer
;    currentLineIndex
;       current line's index
;    currentLine
;       current line
;    nCommands
;       number of space delimited IDL commands on the current line
;    currentCommandIndex
;       index of the current command on the current line
;-
pro docparprofiletokenizer__define
  compile_opt strictarr, hidden
  
  define = { DOCparProFileTokenizer, $
             pLines: ptr_new(), $
             nLines: 0L, $
             currentLineIndex: 0L, $
             currentLine: '', $
             nCommands: 0L, $
             currentCommandIndex: 0L $
           }
end
