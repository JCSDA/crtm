PRO WPAINT_EVENT, EVENT

;- Get information structure
widget_control, event.top, get_uvalue=infoptr
info = *infoptr

;- Get widget identity
widget_control, event.id, get_uvalue=widget

if (info.pen eq 0) then $
  print, 'Event detected in top level base ', event.top

;- Handle events
case widget of

  'Draw' : begin

    ;- Handle button press events (start drawing)
    if (event.press gt 0) then begin
      widget_control, event.id, draw_motion_events=1
      info.pen = 1
      info.x = event.x
      info.y = event.y
    endif

    ;- Handle button release events (stop drawing)
    if (event.release gt 0) then begin
      widget_control, event.id, draw_motion_events=0
      info.pen = 0
    endif

    ;- If pen is down, draw from old location
    ;- to new location
    if (info.pen eq 1) then begin
      wset, info.winid
      plots, [info.x, event.x], [info.y, event.y], $
        /device, thick=2
      info.x = event.x
      info.y = event.y
    endif

    ;- Update the label widget
    label_text = string(info.x, info.y, info.pen, $
      format='("X:", i3, 1x, "Y:", i3, 1x, "Pen:", i1)')
    widget_control, info.label, set_value=label_text

  end

  'Erase' : begin
    print, 'Erasing'
    wset, info.winid
    erase
  end

  else : print, 'Unrecognized event: ', widget

endcase

;- Update state information
*infoptr = info

END

PRO WPAINT_EXIT, EVENT

print, 'Destroying widgets'
widget_control, event.top, /destroy

END

PRO WPAINT_CLEANUP, ID

print, 'Cleaning up'
widget_control, id, get_uvalue=infoptr
ptr_free, infoptr

END

PRO WPAINT

print, 'Creating widgets'

;- Create base widget
device, get_screen_size=screen_size
xoffset = screen_size[0] / 5
yoffset = screen_size[1] / 5
tlb = widget_base(column=1, title='Paint', $
  xoffset=xoffset, yoffset=yoffset, tlb_frame_attr=1)

;- Create draw and button widgets
draw = widget_draw(tlb, xsize=400, ysize=400, $
  uvalue='Draw', /button_events)
label = widget_label(tlb, value='X: Y: Pen:', $
  /align_center, /dynamic_resize)
base = widget_base(tlb, row=1, /align_center)
buttsize = 75
butt = widget_button(base, value='Erase', $
  uvalue='Erase', xsize=buttsize)
butt = widget_button(base, value='Exit', $
  uvalue='Exit', xsize=buttsize, $
  event_pro='wpaint_exit')

;- Realize widgets and get draw window index
widget_control, tlb, /realize
widget_control, draw, get_value=winid
wset, winid
erase

;- Create and store information structure
info = {winid:winid, label:label, pen:0, x:-1L, y:-1L}
infoptr = ptr_new(info)
widget_control, tlb, set_uvalue=infoptr

;- Start managing events
xmanager, 'wpaint', tlb, $
  cleanup='wpaint_cleanup', /no_block

print, 'Done creating widgets'

END
