PRO DRAW_EXAMPLE

;- Create row-aligned non-resizeable top level base
base = widget_base(row=1, title='Draw Example', $
  tlb_frame_attr=1)

;- Create non-scrolling draw widget
draw1 = widget_draw(base, xsize=400, ysize=300)

;- Create scrolling draw widget
draw2 = widget_draw(base, xsize=900, ysize=900, $
  x_scroll_size=300, y_scroll_size=300)

;- Realize widgets
widget_control, base, /realize

;- Get draw window indices
widget_control, draw1, get_value=winid1
widget_control, draw2, get_value=winid2

;- Create a plot in each draw widget
wset, winid1
shade_surf, dist(32), charsize=1.5
wset, winid2
plot, sin(findgen(200) * 0.1)

END
