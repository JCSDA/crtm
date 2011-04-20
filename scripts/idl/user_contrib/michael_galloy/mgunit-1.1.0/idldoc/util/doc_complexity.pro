; docformat = 'rst'

;+
; Computes the cyclomatic complexity (or conditional complexity) of a section
; of code.
;
; The complexity is::
;
;    p - s + 2
;
; where p is the number of decision points and s is the number of exit points.
;
; For more information, see::
;
;    http://en.wikipedia.org/wiki/Cyclomatic_complexity
;
; :Todo:
;    * should ignore comments and string literals
;    * should add the number of non-else clauses in CASE and SWITCH statements
;
; :Returns:
;    long
;
; :Params:
;    lines : in, optional, type=strarr
;       lines of code to analyze
;-
function doc_complexity, lines
  compile_opt strictarr
  
  pattern = '[[:>:]]'
  ;pattern = '[[:space:](),-\+]'
  tokenizer = obj_new('MGffTokenizer', lines, /string_array, pattern=pattern)
  complexity = 1L
  
  while (~tokenizer->done()) do begin
    tok = tokenizer->next(pre_delim=pre, post_delim=post, newline=newline)
    case strlowcase(strtrim(tok, 2)) of
      'if': complexity++
      'case': complexity++   ; really should add the number of non-else cases
      'switch': complexity++ ; really should add the number of non-else cases
      'while': complexity++
      'repeat': complexity++
      'for': complexity++
      'return': complexity--
      else:
    endcase
  endwhile
  
  obj_destroy, tokenizer
  return, complexity > 1
end


; main-level example program

print, doc_complexity(['if(1)then print, 1 else if-1 gt 2 then print, 2 else if+2 gt 3 then print, 3'])
print, doc_complexity(['case 1 of', 'a:print, a', 'b: print, b', 'else:', 'endcase'])

end
