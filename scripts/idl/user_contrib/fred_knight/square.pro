;+
; Name:
;	square
; Purpose:
;	Make a square region for plotting with the current device.
; Usage:
;	!p.position = square()		; PERMANENTLY SQUARE
;	data = [0,1]
;	plot,data,position=square()	; SQUARE ONLY FOR THIS PLOT
;	plot,data,pos=square(/center)	; DITTO & CENTERED IN WINDOW
;	!p.position = square(/top)	; AT TOP MARGIN OF WINDOW
;	!p.position = square(/bottom)	; AT BOTTOM MARGIN OF WINDOW
;	!p.position = square(/left)	; AT LEFT MARGIN OF WINDOW
;	!p.position = square(/right)	; AT RIGHT MARGIN OF WINDOW
;	!p.position = square(size=.5)	; HALF THE LARGEST SIZE POSSIBLE
; Optional Inputs:
;	position = 4-element array specifying [xll,yll,xur,yur] device coords
; Optional Keywords:
;	/help = flag to print header
;	/left = flag to place square at left margin
;	/center = flag to center the square in the plot window
;	/right = flag to place square at right margin
;	/top = like right only for vertical direction
;	/bottom = like left only for vertical direction
;	size = fraction of largest square possible
; Outputs:
;	position = 4-element array giving [xll,yll,xur,yur] device coords
; Common blocks:
;	none
; Procedure:
;	If keyword help is set, call doc_library to print header.
; Restrictions:
; Side Effects:
;	If position is omitted, then a plot is necessary and some plotting
;	keywords will be altered, e.g., !x.range and !y.range.  This is only
;	a momentary change---until the next plot is done.
; Modification history:
;	write, 7 Jul 92, F K Knight (knight@ll.mit.edu)
;	add /top, /bottom, /left (D), /right, & /center keywords, 10 Jul 92, FKK
;-
function square,help=help,position $
  ,left=left,center=center,right=right,top=top,bottom=bottom,size=size
;
;	=====>> HELP
;
on_error,2
if keyword_set(help) then begin & doc_library,'square' & return,0 & endif
;
;	=====>> SET DEFAULTS, EITHER FROM INPUT OR BY PLOTTING
;
if n_elements(position) eq 0 then position = [!x.window(0),!y.window(0),!x.window(1),!y.window(1)]
if (position(0) eq position(2)) or (position(1) eq position(3)) then begin
  plot,/nodata,xstyle=4,ystyle=4,[0],/noerase
;	next line takes care of weirdness with small windows
  position = [min(!x.window),min(!y.window),max(!x.window),max(!y.window)]
  endif
if keyword_set(size) then size = size < 1. > 0. else size = 1.
if size eq 0. then message,'Can''t make a square with 0 width.'
;
;	=====>> CREATE LARGEST SQUARE WITHIN MARGINS OF CURRENT WINDOW
;
xwd = (position(2) - position(0))
ywd = (position(3) - position(1))
aspect = float(!d.x_size)/!d.y_size
if (xwd*!d.x_size) lt (ywd*!d.y_size) then begin
  ywd = xwd * aspect
endif else begin
  xwd = ywd / aspect
endelse
xwd = xwd * size
ywd = ywd * size
;
;	=====>> POSITION SQUARE ACCORDING TO KEYWORDS
;
xlo = position(0)			; DEFAULT AT LOWER LEFT
ylo = position(1)			; DEFAULT AT LOWER LEFT
if keyword_set(center) then begin	; CENTERED VERTICALLY AND HORIZONTALLY
  xlo = (1. - xwd) / 2.
  ylo = (1. - ywd) / 2.
  endif
if keyword_set(right) then xlo = position(2) - xwd
if keyword_set(top) then ylo = position(3) - ywd
if keyword_set(left) then xlo = position(0)
if keyword_set(bottom) then ylo = position(1)
xhi = xlo + xwd
yhi = ylo + ywd
return,[xlo,ylo,xhi,yhi]
end
