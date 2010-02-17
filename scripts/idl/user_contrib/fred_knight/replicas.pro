;+
; Name:
;	replicas
; Purpose:
;	Replicate an array, as replicate.pro does for scalars or structures.
;	Replicas calls replicate if input is a scalar or structure. 
; Usage:
;	array = replicas(/help)
;	copies = replicas(array,ncopies)
; Inputs:
;	array = array to replicate
;	ncopies = number of replicas
; Optional Inputs or Keywords:
;	help = flag to print header
; Outputs:
;	copies = an array of one dimension more than array and filled
;		with copies of array
; Common blocks:
;	none
; Procedure:
;	If keyword help is set, call doc_library to print header.
;	This routine could be incorporated into a more general replicate.
;	However, the use of the parameter ncopies is not as general as
;	in replicate.
; Modification history:
;	write, 22 Feb 92, F.K.Knight
;	guard against calling replicate with an array of structures, 18 Nov 92, FKK
;-
function replicas,array,ncopies,help=help
;
;	=====>> HELP
;
on_error,2
if keyword_set(help) then begin & doc_library,'replicas ' & return,-1 & endif
;
;	=====>> SETUP
;
if ncopies le 0 then message,'Number of replicas ('+strtrim(ncopies,2)+') must be positive.'
sa = size(array)
type = sa(sa(0)+1)
;
;	=====>> IF SCALAR OR STRUCTURE, THEN USE REPLICATE.
;
if sa(0) eq 0 or ((sa(1) eq 1) and (type eq 8)) then $
  return, replicate(array,ncopies)
;
;	=====>> TREAT ARRAY OF STRUCTURES: CAN'T FIGURE OUT HOW TO DO IT
;
if type eq 8 then message,'Cannot do array of structures.'
;
;	=====>> OTHERWISE, REPLICATE USING make_array.
;
na = n_elements(array)
sc = [sa(0)+1,sa(1:sa(0)),ncopies,type,ncopies*na]
copies = make_array(size=sc)
for i = 0,ncopies-1 do copies(na*i) = array
return,copies
end
