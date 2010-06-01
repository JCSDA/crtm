PRO IMGUI_DISPLAY, INFO

if info.debug then print, 'IMGUI_DISPLAY'

;- Load color table and display image
widget_control, info.tlb, hourglass=1
tvlct, info.red, info.grn, info.blu, info.bottom
if (!d.name eq info.device) then begin
  wset, info.draw_window
  erase, info.bottom
endif
imdisp, info.image, bottom=info.bottom, $
  ncolors=info.ncolors, out_pos=out_pos, margin=0.0
info.out_pos = out_pos
widget_control, info.tlb, hourglass=0

END
