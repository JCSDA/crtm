PRO IMGUI_SET_STATE, EVENT, INFO, NO_COPY=NO_COPY

;- Get pointer
widget_control, event.top, get_uvalue=infoptr
if (ptr_valid(infoptr) eq 0) then $
  message, 'State information pointer is invalid'

;- Set state information structure
if (n_elements(info) eq 0) then $
  message, 'State information structure is undefined'
if keyword_set(no_copy) then begin
  *infoptr = temporary(info)
endif else begin
  *infoptr = info
endelse

END
