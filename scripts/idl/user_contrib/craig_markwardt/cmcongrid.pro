; $Id$

;+
; NAME:
;	CMCONGRID
;
; PURPOSE:
;       Shrink or expand the size of an array by an arbitrary amount.
;       This IDL procedure simulates the action of the VAX/VMS
;       CONGRID/CONGRIDI function.
;
;	This function is similar to "REBIN" in that it can resize a
;       one, two, or three dimensional array.   "REBIN", however,
;       requires that the new array size must be an integer multiple
;       of the original size.   CONGRID will resize an array to any
;       arbitrary size (REBIN is somewhat faster, however).
;       REBIN averages multiple points when shrinking an array,
;       while CONGRID just resamples the array.
;
; CATEGORY:
;       Array Manipulation.
;
; CALLING SEQUENCE:
;	array = CONGRID(array, x, y, z)
;
; INPUTS:
;       array:  A 1, 2, or 3 dimensional array to resize.
;               Data Type : Any type except string or structure.
;
;       x:      The new X dimension of the resized array.
;               Data Type : Int or Long (greater than or equal to 2).
;
; OPTIONAL INPUTS:
;       y:      The new Y dimension of the resized array.   If the original
;               array has only 1 dimension then y is ignored.   If the
;               original array has 2 or 3 dimensions then y MUST be present.
;
;       z:      The new Z dimension of the resized array.   If the original
;               array has only 1 or 2 dimensions then z is ignored.   If the
;               original array has 3 dimensions then z MUST be present.
;
; KEYWORD PARAMETERS:
;       INTERP: If set, causes linear interpolation to be used.
;               Otherwise, the nearest-neighbor method is used.
;
;	CUBIC:	If set, uses "Cubic convolution" interpolation.  A more
;		accurate, but more time-consuming, form of interpolation.
;		CUBIC has no effect when used with 3 dimensional arrays.
;
;       MINUS_ONE:
;               If set, will prevent CONGRID from extrapolating one row or
;               column beyond the bounds of the input array.   For example,
;               If the input array has the dimensions (i, j) and the
;               output array has the dimensions (x, y), then by
;               default the array is resampled by a factor of (i/x)
;               in the X direction and (j/y) in the Y direction.
;               If MINUS_ONE is present (AND IS NON-ZERO) then the array
;               will be resampled by the factors (i-1)/(x-1) and
;               (j-1)/(y-1).
;
;       HALF_HALF:
;               If set, will tell CONGRID to extrapolate a *half* row
;               and column on either side, rather than the default of
;               one full row/column at the ends of the array.  If you
;               are interpolating images with few rows, then the
;               output will be more consistent with this technique.
;               This keyword is intended as a replacement for
;               MINUS_ONE, and both keywords probably should not be
;               used in the same call to CONGRID.
;
; OUTPUTS:
;	The returned array has the same number of dimensions as the original
;       array and is of the same data type.   The returned array will have
;       the dimensions (x), (x, y), or (x, y, z) depending on how many
;       dimensions the input array had.
;
; PROCEDURE:
;       IF the input array has three dimensions, or if INTERP is set,
;       then the IDL interpolate function is used to interpolate the
;       data values.
;       If the input array has two dimensions, and INTERP is NOT set,
;       then the IDL POLY_2D function is used for nearest neighbor sampling.
;       If the input array has one dimension, and INTERP is NOT set,
;       then nearest neighbor sampling is used.
;
; EXAMPLE:
;       ; vol is a 3-D array with the dimensions (80, 100, 57)
;       ; Resize vol to be a (90, 90, 80) array
;       vol = CONGRID(vol, 90, 90, 80)
;
; MODIFICATION HISTORY:
;       DMS, Sept. 1988.
;       DMS, Added the MINUS_ONE keyword, Sept. 1992.
; 	Daniel Carr. Re-wrote to handle one and three dimensional arrays
;                    using INTERPOLATE function.
; 	DMS, RSI, Nov, 1993.  Added CUBIC keyword.
;       Craig Markwardt, Dec, 1997.  Added halfhalf keyword to
;                        more evenly distribute "dead" pixel row
;       Use uniformly spaced grid points for half_half W. Landsman   Feb. 2000
;              (and slightly modified by C. Markwardt 14 Feb 2000)
;       Fix in case where INTERP=0 (nearest neighbor interp) and
;         expanding the image (thanks to Larry Bradley) 28 Mar 2007
;
;  $Id$
;-

FUNCTION CMCONGRID, arr, x, y, z, Interp=int, Minus_One=m1, Cubic = cubic, $
                    Half_Half=hh

ON_ERROR, 2		;Return to caller if error
s = Size(arr)

IF ((s(0) EQ 0) OR (s(0) GT 3)) THEN $
   Message, 'Array must have 1, 2, or 3 dimensions.'

;  Supply defaults = no interpolate, and no minus_one.
if n_elements(int) le 0 then int = 0 else int = keyword_set(int)
if n_elements(m1) le 0 then m1 = 0 else m1 = keyword_set(m1)

; Compute offsets pixel offsets for half_half
halfx = 0.0 & halfy = 0.0 & halfz = 0.0
if keyword_set(hh) then begin
    if s(0) GE 1 then halfx = -0.5 + (float(s(1))/x)
    if s(0) GE 2 then halfy = -0.5 + (float(s(2))/y)
    if s(0) GE 3 then halfz = -0.5 + (float(s(3))/z)
endif
cub = KEYWORD_SET(cubic)
if cub THEN int = 1	;Cubic implies interpolate
	

CASE s(0) OF
   1: BEGIN				; *** ONE DIMENSIONAL ARRAY
	srx = float(s(1) - m1)/(x-m1) * findgen(x) + halfx
      IF int THEN $
         RETURN, INTERPOLATE(arr, srx, CUBIC = cub) ELSE $
         RETURN, arr(ROUND(srx))
   ENDCASE
   2: BEGIN ; *** TWO DIMENSIONAL ARRAY
	IF int THEN BEGIN
	  srx = float(s(1) - m1) / (x-m1) * findgen(x) + halfx
	  sry = float(s(2) - m1) / (y-m1) * findgen(y) + halfy
          RETURN, INTERPOLATE(arr, srx, sry, /GRID, CUBIC=cub)
	ENDIF ELSE BEGIN
          ;; match IDL's CONGRID function
          expand = (x gt s[1])
          xm1 = (m1 or expand) ? x-1 : x
          RETURN, POLY_2D(arr, $
               [[0,0],[(s(1)-m1)/float(xm1),0]], $ ;Use poly_2d
               [[0,(s(2)-m1)/float(y-m1)],[0,0]],int,x,y)
        ENDELSE

   ENDCASE
   3: BEGIN ; *** THREE DIMENSIONAL ARRAY
	srx = float(s(1) - m1) / (x-m1) * findgen(x) + halfx
	sry = float(s(2) - m1) / (y-m1) * findgen(y) + halfy
	srz = float(s(3) - m1) / (z-m1) * findgen(z) + halfz
	RETURN, interpolate(arr, srx, sry, srz, /grid)
   ENDCASE
ENDCASE

RETURN, arr_r
END
