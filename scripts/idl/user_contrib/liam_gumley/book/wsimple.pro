PRO WSIMPLE_EVENT, EVENT

print, 'Event detected'

;- Display event structure
help, event, /structure

END

PRO WSIMPLE

print, 'Creating widgets'

;- Create top level base
device, get_screen_size=screen_size
xoffset = screen_size[0] / 3
yoffset = screen_size[1] / 3
tlb = widget_base(column=1, tlb_frame_attr=1, $
  xoffset=xoffset, yoffset=yoffset)

;- Create widgets
text = widget_text(tlb, value='Edit this text', /editable)
base = widget_base(tlb, row=1, /align_center)
button = widget_button(base, value='Click here')
widget_control, base, /realize

;- Start managing events
xmanager, 'wsimple', tlb, /no_block

END
