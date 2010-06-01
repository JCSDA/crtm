PRO IMGUI_TLB, EVENT

;- Get state information
imgui_get_state, event, info

if info.debug then print, 'IMGUI_TLB'

;- Get change in size of top level base
if (info.version lt 5.4) then begin
  xchange = event.x - info.base_size[0]
  ychange = event.y - info.base_size[1]
endif else begin
  widget_control, event.id, tlb_get_size=base_size
  xchange = base_size[0] - info.base_size[0]
  ychange = base_size[1] - info.base_size[1]
endelse

;- Set new size of draw window
info.draw_xsize = (info.draw_xsize + xchange) > 200
info.draw_ysize = (info.draw_ysize + ychange) > 200
widget_control, info.draw_id, $
  xsize=info.draw_xsize, ysize=info.draw_ysize

;- Store new top level base size and display image
widget_control, event.top, tlb_get_size=base_size
info.base_size = base_size
imgui_display, info

;- Set state information
imgui_set_state, event, info

END
