PRO IMGUI_SAVEIMAGE, EVENT

;- Get state information
imgui_get_state, event, info

if info.debug then print, 'IMGUI_SAVEIMAGE'

;- Save a bitmap image
filename = dialog_pickfile(/write, group=event.top)
if (filename ne '') then begin
  widget_control, event.top, hourglass=1
  widget_control, event.id, get_uvalue=widget
  wset, info.draw_window
  case widget of
    'JPEG' : saveimage, filename, /quiet, /jpeg
    'TIFF' : saveimage, filename, /quiet, /tiff
  endcase
  widget_control, event.top, hourglass=0
endif

END
