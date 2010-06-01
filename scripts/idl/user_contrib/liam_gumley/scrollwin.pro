pro scrollwin, nx, ny, sx, sy, title = title, group = group

;+
; Purpose:
;     Create a scrolling draw window of specified size.
;
; Calling Sequence:
;     SCROLLWIN, NX, NY, SX, SY, TITLE = TITLE, GROUP = GROUP
;
; Input:
;     NX       Horizontal size of window (pixels).
;     NY       Vertical size of window (pixels).
;     SX       Visible horizontal window size (pixels).
;     SY       Visible vertical window size (pixels).
;
; Optional Keywords:
;     TITLE    Title string for the window.
;     GROUP    The widget ID of the widget that calls SCROLLWIN.
;              When this ID is specified, a death of the caller results
;              in the death of the SCROLLWIN draw window widget.
;
; Revised:
;     24-JULY-1996 Liam Gumley, CIMSS/SSEC
;-

;- check keywords

if n_elements( title ) eq 0 then title = "SCROLLWIN Draw Window"
if n_elements( group ) eq 0 then group = 0

;- create and realize draw widget

root = widget_base( title = title, group = group )
base = widget_base( root, column = 1 )
draw = widget_draw( base, xs = nx, ys = ny, x_sc = sx, y_sc = sy, /scroll )
widget_control, base, /realize

end
