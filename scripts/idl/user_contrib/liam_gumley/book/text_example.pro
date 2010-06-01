PRO TEXT_EXAMPLE

;- Create column-aligned non-resizeable top level base
tlb = widget_base(column=1, title='Text Example', $
  tlb_frame_attr=1)

;- Create label widget
label1 = widget_label(tlb, value='Single line text widget')

;- Create single line editable text widget
text1 = widget_text(tlb, value='Default text', /editable)

;- Create label widget
label2 = widget_label(tlb, value='Multi line text widget')

;- Create multi line text widget
value = ['Text line 1', 'Text line 2', 'Text line 3']
text2 = widget_text(tlb, value=value, ysize=3)

;- Realize widgets
widget_control, tlb, /realize

END
