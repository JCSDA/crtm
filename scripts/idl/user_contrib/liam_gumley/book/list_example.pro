PRO LIST_EXAMPLE

;- Create row-aligned non-resizeable top level base
tlb = widget_base(row=1, title='List Example', $
  tlb_frame_attr=1)

;- Create single selection list widget
colors = ['Red', 'Green', 'Blue', 'Cyan', $
  'Yellow', 'Magenta', 'Black', 'White', $
  'Gray', 'Navy', 'Brown', 'Gold']
list1 = widget_list(tlb, value=colors, ysize=6)

;- Create multiple selection list widget
months = ['January', 'February', 'March', 'April', $
  'May', 'June', 'July', 'August', $
  'September', 'October', 'November', 'December']
list2 = widget_list(tlb, value=months, ysize=6, /multiple)

;- Realize widgets
widget_control, tlb, /realize

END
