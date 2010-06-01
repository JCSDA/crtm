pro box, x, y, width, height, color = color

;+
; Purpose: Draw a box on the display in data coordinates
;
; Input:
;       x         x coordinate of center of box
;       y         y coordinate of center of box
;       width     width of box in x direction
;       height    height of box in y direction
;
; Optional keywords:
;       color      color index (default = 0)
;
; Example:
; PLOT, [ 0 ]
; BOX, 0.5, 0.5, 0.25, 0.25, COLOR = 255
;-

xmin = x - width * 0.5
xmax = x + width * 0.5
ymin = y - height * 0.5
ymax = y + height * 0.5
if not keyword_set( color ) then color = 0
plots, [ xmin, xmin ], [ ymin, ymax ], color = color, /data
plots, [ xmin, xmax ], [ ymax, ymax ], color = color, /data
plots, [ xmax, xmax ], [ ymax, ymin ], color = color, /data
plots, [ xmax, xmin ], [ ymin, ymin ], color = color, /data

end
