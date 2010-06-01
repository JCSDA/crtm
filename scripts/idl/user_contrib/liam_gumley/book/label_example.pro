PRO LABEL_EXAMPLE

;- Create column aligned non-resizable top level base
tlb = widget_base(column=1, title='Label Example', $
  tlb_frame_attr=1)

;- Create label widgets
label1 = widget_label(tlb, value='This is a long label')
label2 = widget_label(tlb, value='Left', /align_left)
label3 = widget_label(tlb, value='Center', /align_center)
label4 = widget_label(tlb, value='Right', /align_right)

;- Realize widgets
widget_control, tlb, /realize

END
