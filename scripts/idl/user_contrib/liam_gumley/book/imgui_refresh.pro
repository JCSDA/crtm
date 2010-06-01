PRO IMGUI_REFRESH, EVENT

;- Get state information
imgui_get_state, event, info

if info.debug then print, 'IMGUI_REFRESH'

;- Refresh image
imgui_display, info

END
