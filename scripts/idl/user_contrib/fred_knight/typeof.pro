;+
; Name:
;	typeof
; Purpose:
;	Function to return the type of the variable---a shorthand
;	for extracting the type from the array returned by size.
; Usage:
;	if typeof(variable) eq 7 then message,'Variable is a string.'
; Inputs:
;	variable = any IDL variable
; Optional Inputs or Keywords:
;	help = flag to print header
; Outputs:
;	typeof = type code from the size array
; Common blocks:
;	none
; Procedure:
;	Just get the type code from the size array.
; Modification history:
;	write, 2 Dec 92, F.K.Knight (knight@ll.mit.edu)
;-
function typeof,variable,help=help
;
;	=====>> HELP
;
on_error,2
if keyword_set(help) then begin & doc_library,'typeof' & return,0 & endif
;
;	=====>> RETURN THE TYPE CODE
;
szv = size(variable)
return,szv(szv(0)+1)
end
