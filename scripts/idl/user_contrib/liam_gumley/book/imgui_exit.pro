PRO IMGUI_EXIT, EVENT

;- Get state information
imgui_get_state, event, info

if info.debug then print, 'IMGUI_EXIT'

;- Destroy the top level base
widget_control, event.top, /destroy

END
