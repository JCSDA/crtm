PRO IMGUI_GET_STATE, EVENT, INFO, NO_COPY=NO_COPY

;- Get pointer
widget_control, event.top, get_uvalue=infoptr
if (ptr_valid(infoptr) eq 0) then $
  message, 'State information pointer is invalid'

;- Get state information structure
if (n_elements(*infoptr) eq 0) then $
  message, 'State information structure is undefined'
if keyword_set(no_copy) then begin
  info = temporary(*infoptr)
endif else begin
  info = *infoptr
endelse

END
