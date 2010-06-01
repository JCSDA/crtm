PRO IMGUI_COLORS, EVENT

;- Get state information
imgui_get_state, event, info

if info.debug then print, 'IMGUI_COLORS'

;- Start color table widget
if (info.version ge 5.2) then begin
  tvlct, info.red, info.grn, info.blu, info.bottom
  xloadct, bottom=info.bottom, ncolors=info.ncolors, $
    group=event.top, updatecallback='imgui_xloadct', $
    updatecbdata=event
endif else begin
  result = dialog_message('IDL 5.2 or higher required', $
    dialog_parent=event.id)
endelse

END
