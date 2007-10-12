;+
; NAME:
;     Colors
;
; PURPOSE:
;     Load the sixteen standard graphics colors into the current color table.
;
; CALLING SEQUENCE:
;     Colors, START = Start
;
; KEYWORD ARGUMENTS:
;     Start:   Start index in the color table where the graphics
;              colors will be loaded (default = 0).
;
; COMMENTS:
;     The color table assignments are as follows
;      0 => black
;      1 => magenta
;      2 => cyan
;      3 => yellow
;      4 => green
;      5 => red
;      6 => blue
;      7 => white
;      8 => navy
;      9 => gold
;     10 => pink
;     11 => aquamarine
;     12 => orchid
;     13 => gray
;     14 => sky
;     15 => beige
;
;     Same as Liam Gumley's colors.pro, but with some different colours.
;
;-
PRO Colors, START=Start
    
  ; Check keywords
  IF ( N_ELEMENTS(Start) EQ 0 ) THEN Start = 0

  ; Load the graphics colour table
  ;      0    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15
  r = [  0, 255,   0, 255,   0, 255,   0, 255,   0, 255, 255, 112, 219, 127,   0, 255 ]
  g = [  0,   0, 255, 255, 255,   0,   0, 255,   0, 187, 127, 219, 112, 127, 163, 171 ]
  b = [  0, 255, 255,   0,   0,   0, 255, 255, 115,   0, 127, 147, 219, 127, 255, 127 ]
  TVLCT, r, g, b, Start

END
