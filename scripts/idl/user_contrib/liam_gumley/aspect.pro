;+
; NAME:
;	ASPECT
;
; PURPOSE:
;	This function calculates and returns the normalized position
;	coordinates necessary to put a plot with a specified aspect ratio
;	into the currently active graphics window. It works on the display
;	output window as well as in a PostScript output window.
;
; CATEGORY:
;	Graphics
;
; CALLING SEQUENCE:
;
;	position = ASPECT(aspectRatio)
;
; INPUTS:
;	aspectRatio: A floating point value that is the desired aspect
;	ratio (ratio of heigth to width) of the plot in the current 
;	graphics output window. If this parameter is missing, an aspect
;	ratio of 1.0 (a square plot) is assumed.
;
; KEYWORD PARAMETERS:
;	MARGIN:	The margin around the edges of the plot. The value must be
;	a floating point value between 0.0 and 0.5. It is expressed in
;	normalized coordinate units.
;
; OUTPUTS:
;	position: A four-element floating array of normalized coordinates.
;	The order of the elements is [x0, y0, x1, y1], similar to the
;	!P.POSITION system variable or the POSITION keyword on any IDL
;	graphic command.
;
; EXAMPLE:
;	To create a plot with an aspect ratio of 1:2 and a margin of
;	0.10 around the edge of the output window, do this:
;
;	   plotPosition = ASPECT(0.5, Margin=0.10)
;	   PLOT, Findgen(11), POSITION=plotPosition
;	
;	Notice this can be done in a single IDL command, like this:
;	
;	   PLOT, Findgen(11), POSITION=ASPECT(0.5, Margin=0.10)
;
; MODIFICATION HISTORY:
; 	Written by:	David Fanning, November 1996.
;       Added better error checking, 18 Feb 97, DWF.
;-

FUNCTION ASPECT, aspectRatio, MARGIN=margin

ON_ERROR, 1

   ; Check for aspect ratio parameter and possibilities.
   
IF N_PARAMS() EQ 0 THEN aspectRatio = 1.0

IF aspectRatio EQ 0 THEN BEGIN
   MESSAGE, 'Aspect Ratio of 0. Changing to 1...', /Informational
   aspectRatio = 1.0
ENDIF

s = SIZE(aspectRatio)
IF s(s(0)+1) NE 4 THEN $
   MESSAGE, 'Aspect Ratio is not a FLOAT. Take care...', /Informational

   ; Check for margins.
   
xmargin = fltarr( 2 )
ymargin = fltarr( 2 )   
if keyword_set( margin ) then begin
  xmargin( * ) = margin
  ymargin( * ) = margin
endif else begin
  xmargin(0) = ( float( !d.x_ch_size ) / float( !d.x_vsize ) ) * !x.margin(0)
  xmargin(1) = ( float( !d.x_ch_size ) / float( !d.x_vsize ) ) * !x.margin(1)
  ymargin(0) = ( float( !d.y_ch_size ) / float( !d.y_vsize ) ) * !y.margin(0)
  ymargin(1) = ( float( !d.y_ch_size ) / float( !d.y_vsize ) ) * !y.margin(1)
endelse

   ; Calculate the aspect ratio of the current window.
   
wAspectRatio = FLOAT(!D.Y_VSIZE) / !D.X_VSIZE

   ; Calculate normalized positions in window.
   
IF (aspectRatio LE wAspectRatio) THEN BEGIN
   xstart = xmargin(0)
   ystart = 0.5 - (0.5 - ymargin(0)) * (aspectRatio / wAspectRatio)
   xend = 1.0 - xmargin(1)
   yend = 0.5 + (0.5 - ymargin(1)) * (aspectRatio / wAspectRatio)
ENDIF ELSE BEGIN
   xstart = 0.5 - (0.5 - xmargin(0)) * (wAspectRatio / aspectRatio)
   ystart = ymargin(0)
   xend = 0.5 + (0.5 - xmargin(1)) * (wAspectRatio / aspectRatio)
   yend = 1.0 - ymargin(1)
ENDELSE

position = [xstart, ystart, xend, yend]

RETURN, position
END
