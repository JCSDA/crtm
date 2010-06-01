PRO WGETNAME_EVENT, EVENT

print, 'Event detected'

;- Get state information
widget_control, event.top, get_uvalue=infoptr
info = *infoptr

;- Identify widget which caused the event
widget_control, event.id, get_uvalue=widget
help, widget

;- Handle events
if (widget eq 'Text') or (widget eq 'OK') then begin
  widget_control, info.text, get_value=name
  info.name = name
  info.status = 'OK'
endif else begin
  info.name = ''
  info.status = 'Cancel'
endelse

;- Save state information
*infoptr = info

;- Destroy the widget hierarchy
widget_control, event.top, /destroy

END

PRO WGETNAME_CLEANUP, ID

print, 'Cleaning up'

;- Get state information
widget_control, id, get_uvalue=infoptr
info = *infoptr

;- Save result
result = {name:info.name, status:info.status}
*infoptr = result

END

PRO WGETNAME, NAME, STATUS

print, 'Creating widgets'

;- Create top level base
device, get_screen_size=screen_size
xoffset = screen_size[0] / 3
yoffset = screen_size[1] / 3
tlb = widget_base(column=1, title='Name', $
  xoffset=xoffset, yoffset=yoffset, tlb_frame_attr=1)

;- Create text entry widgets
tbase = widget_base(tlb, row=1)
label = widget_label(tbase, value='Enter your name: ')
text  = widget_text(tbase, uvalue='Text', /editable)

;- Create button widgets
bbase = widget_base(tlb, row=1, /align_center)
bsize = 75
butta = widget_button(bbase, value='OK', $
  uvalue='OK', xsize=bsize)
buttb = widget_button(bbase, value='Cancel', $
  uvalue='Cancel', xsize=bsize)

;- Realize widgets
widget_control, tlb, /realize

;- Create and store state information
info = {text:text, name:'', status:'Cancel'}
infoptr = ptr_new(info)
widget_control, tlb, set_uvalue=infoptr

;- Manage events
xmanager, 'wgetname', tlb, cleanup='wgetname_cleanup'

;- Get result
result = *infoptr
ptr_free, infoptr
name = result.name
status = result.status

END
