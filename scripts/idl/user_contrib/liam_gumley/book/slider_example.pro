PRO SLIDER_EXAMPLE

;- Create column-aligned non-resizeable top level base
tlb = widget_base(column=1, title='Slider Example', $
  tlb_frame_attr=1)

;- Create slider widgets
width = 200
slider1 = widget_slider(tlb, value=45, min=-90, max=90, $
  title='Latitude', xsize=width)
slider2 = widget_slider(tlb, value=-90, min=-180, max=180, $
  title='Longitude', xsize=width)

;- Realize widgets
widget_control, tlb, /realize

END