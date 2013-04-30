;+========================================================================
; :Author: Matt Savoie  <savoie@nsidc.org>
; :Copyright: (C) <2013> University of Colorado.
; :Version: $Id$
;
; Created 01/11/2013
; National Snow & Ice Data Center, University of Colorado, Boulder
;-========================================================================*/

;+
;  boolean test to see if the file root is in the form of a new nsidc0046
;  file.
;  "EASE2_N25km.snowice.19661003-19661009.v04.bin"
;
; :Params:
;    filename : in, required, type=string
;       input filename root.
;
; :Returns: 1 if the file is a new style nsidc0046 file, 0 otherwise
;
;-
FUNCTION is_nsidc0046,  rootname
    COMPILE_OPT idl2, logical_predicate

    regex =  "EASE2_N25km.snowice.[0-9]{8}-[0-9]{8}\.v..\.bin"
    RETURN,  stregex( rootname, regex, /boolean )
END
