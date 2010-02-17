;+
; Name:
;	nbytes
; Purpose:
;	Return the number of bytes in the variable
; Usage:
;	nb = nbytes(variable)
; Inputs:
;	variable = any IDL variable
; Optional Inputs or Keywords:
;	help = flag to print header
; Outputs:
;	nb = number of bytes in variable
; Common blocks:
;	none
; Procedure:
;	Idea from David Stern.
; Modification history:
;	write, 22 Feb 92, F.K.Knight
;	increase speed by writing to disk only for structures, 10 Sep 92, FKK
;	eliminate Unix-specific file (from ali@rsinc.com), 11 Sep 92, FKK
;-
function nbytes,variable,help=help
;
;	=====>> HELP
;
on_error,2
if keyword_set(help) then begin & doc_library,'nbytes' & return,-1 & endif
;
;	=====>> CHOOSE OPTION BASED ON TYPE OF VARIABLE
;
sz = size(variable)
type = sz(sz(0)+1)
nelements = sz(sz(0)+2)
case type of
0:return,0				; UNDEFINED
1:return,nelements			; BYTE
2:return,nelements*2			; INT
3:return,nelements*4			; LONG
4:return,nelements*4			; FLOAT
5:return,nelements*8			; DOUBLE
6:return,nelements*8			; COMPLEX
7:return,long(total(strlen(variable)))	; STRING: DOESN'T COUNT NULLS
8:begin					; STRUCTURE: COMPILER-SPECIFIC LENGTH
  file = filepath('IDL',/tmp)
  openw,lun,file,/get_lun,/delete
  writeu,lun,variable(0)		; SO WRITE 1ST ELEMENT TO FILE
  stat = fstat(lun)
  close,lun
  free_lun,lun
  return,stat.size*nelements		; AND RETURN TOTAL LENGTH
  end
else: message, 'unknown type = '+strtrim(type,2)
endcase
end
