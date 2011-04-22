;+
; NAME:
;        LOGLEVELS (function)
;
; PURPOSE:
;        Compute default values for logarithmic axis labeling
;        or contour levels. For a range from 1 to 100 these 
;        would be 1., 2., 5., 10., 20., 50., 100.
;        If the range spans more than (usually) 3 decades, only 
;        decadal values will be returned unless the /FINE keyword 
;        is set. This function was mainly written to overcome IDL's
;        weakness to consider only decadal values as major intervals
;        in log plots.
;
; CATEGORY:
;        General Graphics
;
; CALLING SEQUENCE:
;        result = LOGLEVELS([range | MIN=min,MAX=max] [,/FINE] [,COARSE=dec])
;
; INPUTS:
;        RANGE -> A 2-element vector with the minimum and maximum 
;            value to be returned. Only levels _within_ this range
;            will be returned. If RANGE contains only one element,
;            this is interpreted as MAX and MIN will be assumed as
;            3 decades smaller. RANGE superseeds the MIN and MAX 
;            keywords. Note that RANGE must be positive definite
;            but can be given in descending order in which case
;            the labels will be reversed.
;
; KEYWORD PARAMETERS:
;        MIN, MAX -> alternative way of specifying a RANGE. If only 
;            one keyword is given, the other one is computed as
;            3 decades smaller/larger than the given parameter.
;            RANGE superseeds MIN and MAX.
;
;        FINE (integer) -> control the "grain" of log levels. 
;            FINE = 1 : 1, 2, 5, 10, 20, 50, ...
;            FINE = 2 : 1, 2, 3, 4, 5, 10, 20, 30, ...
;            FINE = 3 : 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30 , ...
;
;        COARSE -> the maximum number of decades for which LOGLEVELS
;            shall return fine labels. Default is 3. (non-integer 
;            values are possible).
;
; OUTPUTS:
;        A vector with "round" logarithmic values within the given 
;        range. The original (or modified) RANGE will be returned
;        unchanged if RANGE does not span at least one label interval.
;        The result will always contain at least two elements.
;
;
; SUBROUTINES:
;        none
;
; REQUIREMENTS:
;        none
;
; NOTES:
;        If COARSE is lt 0, the nearest decades will be returned 
;        instead. The result will always have at least two elements.
;        If COARSE forces decades, the result values may be out-of-
;        range if RANGE spans less than a decade.
;        
;        Caution with type conversion from FLOAT to DOUBLE !!
;
; EXAMPLE:
;        range = [ min(data), max(data) ]
;        c_level = LOGLEVELS(range)
;        contour,...,c_level=c_level
;        
;
; MODIFICATION HISTORY:
;        mgs, 17 Mar 1999: VERSION 1.00
;        mgs, 26 Aug 2000: - changed copyright to open source
;        mgs, 21 Jun 2002: - changed fine from boolean to integer
;                            keyword:
;                            fine=1 : 1, 2, 5, 10, 20, ...
;                            fine=2 : 1, 2, 3, 4, 5, 10, 20, ...
;                            fine=3 : 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
;                            20, 30, ...
;        mgs, 06 Feb 2003: - bug fix for fine=3: decades were
;                            duplicated (thanks to Francis Vitt)
;        mgs, 09 Oct 2003: - bug fix for fine=0 (no matching case)
;
;
;###########################################################################
;
; LICENSE
;
; This software is OSI Certified Open Source Software.
; OSI Certified is a certification mark of the Open Source Initiative.
;
; Copyright © 2000 Martin Schultz
;
; This software is provided "as-is", without any express or
; implied warranty. In no event will the authors be held liable
; for any damages arising from the use of this software.
;
; Permission is granted to anyone to use this software for any
; purpose, including commercial applications, and to alter it and
; redistribute it freely, subject to the following restrictions:
;
; 1. The origin of this software must not be misrepresented; you must
;    not claim you wrote the original software. If you use this software
;    in a product, an acknowledgment in the product documentation
;    would be appreciated, but is not required.
;
; 2. Altered source versions must be plainly marked as such, and must
;    not be misrepresented as being the original software.
;
; 3. This notice may not be removed or altered from any source distribution.
;
; For more information on Open Source Software, visit the Open Source
; web site: http://www.opensource.org.
;
;###########################################################################


function loglevels,range,min=mind,max=maxd,  $
           coarse=coarse,fine=fine
 

    if (n_elements(COARSE) eq 0) then COARSE = 3

    ; Make sure to have a valid range which is positive definite
    ; NOTE that range does not need to be sorted!
    if (n_elements(mind) gt 0) then begin
       mind = mind[0]
       if (n_elements(maxd) eq 0) then maxd = mind*1000.
    endif
    if (n_elements(maxd) gt 0) then begin
       maxd = maxd[0]
       if (n_elements(mind) eq 0) then mind = maxd*0.001
    endif
    ; still not defined, i.e. neither mind nor maxd given
    if (n_elements(mind) eq 0) then begin
       mind = 0.1
       maxd = 100.
    endif
    ; RANGE superseeds min and max 
    if (n_elements(range) eq 0) then range = [ mind,maxd ]
    ; one element for RANGE is interpreted as MAX
    if (n_elements(range) eq 1) then range = [ range*0.001, range ]
 
    thisrange = double(range) > 1.D-100
    thisrange = thisrange(sort(thisrange))
 
    ; set lower range to 3 decades below upper range if it is zero
    if (thisrange[0] lt 1.D-36) then thisrange[0] = thisrange[1]/1000. 


    ; get log of ranges and decadal log
    lrange = alog10(thisrange)
    if (lrange[0] lt 0.) then lrange[0] = lrange[0] - 1.0D-6
    if (lrange[1] lt 0.) then lrange[1] = lrange[1] - 1.0D-6
;   if (lrange[1] gt 0.) then lrange[1] = lrange[1] + 1.0D-6
    drange = fix(lrange)
 
 
    ; create label arrays to choose from
    ; currently 1.D-15 to 5.D+16
    ; ranges outside these limits always return only decades
 
    ; set mode according to following rules:
    ; - range outside limits -> return decades
    ; - coarse exceeded -> return decades
    ; - fine set -> return 1,2,5,... for any number of decades
    ; - [automatic] -> return decades if more than COARSE decades
    ;                 otherwise 1,2,5,..
 
    mode = 0   ; return decades
    IF n_elements(fine) EQ 0 THEN fine = 0
    fine = (fine > 0) < 3
    if fine GT 0 then mode = 1
    if ((lrange[1]-lrange[0]) le COARSE) then mode = 1
    if (thisrange[0] lt 1.D-15 OR thisrange[1] gt 5.D16) then mode = 0
 
    if (mode) then begin
       ; make overall array
       CASE fine OF 
          0 : template = [ 1.D0 ]
          1 : template = [ 1.D0, 2.D0, 5.D0 ]
          2 : template = [ 1.D0, 2.D0, 3.D0, 4.D0, 5.D0 ]
          3 : template = dindgen(9)+1.D0
       ENDCASE 
       labels = template*10.D0^(-16)
       FOR i=-15,16 DO BEGIN
          labels = [ labels, template*10.D0^i ]
       ENDFOR 
       IF fine EQ 3 THEN labels = [ labels, 1.D17 ]
 
        llabels = alog10(labels)
        ind = where(llabels ge lrange[0] AND llabels le lrange[1])
 
        ; if range values are too close, return original range
        if (ind[0] lt 0) then return,range
 
        ; return reversed labels if range[0] gt range[1]
        if (range[0] gt range[1]) then $
           return,reverse(labels[min(ind):max(ind)]) $
        else $
           return,labels[min(ind):max(ind)]
 
    endif else begin

       if (lrange[1] lt 0.) then drange[1] = drange[1] - 1

       exponent = indgen(drange[1]-drange[0]+1)+drange[0]
       if (n_elements(exponent) eq 1) then $
          exponent = [ exponent, exponent+1 ]

       if (range[0] gt range[1]) then $
          return,reverse(10.D0^exponent)  $
       else $
          return,10.D0^exponent
    endelse
 
end
 
 
