pro undefine, varname

;+
;PURPOSE:
;   Delete a variable and make it undefined.
;
;CALLING SEQUENCE:
;   UNDEFINE, VARNAME
;
;INPUT:
;   VARNAME      Named variable to be deleted
;
;CREATED:
;   Liam Gumley, CIMSS/SSEC, 18-FEB-1997
;   liam.gumley@ssec.wisc.edu
;   http://cimss.ssec.wisc.edu/~gumley/index.html
;   (From a suggestion by David Fanning)
;
;EXAMPLE:
;
;;Create a variable, delete it, then demonstrate that it is undefined.
;
;a = 5.0
;undefine, a
;help, a
;-

tempvar = size( temporary( varname ) )

end
