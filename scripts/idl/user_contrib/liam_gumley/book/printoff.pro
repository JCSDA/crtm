PRO PRINTOFF, QUIET=QUIET

;- Check that Printer output is active
if (!d.name ne 'PRINTER') then begin
  message, 'PRINTER output not active: ' + $
    'nothing done', /continue
  return
endif

;- Get entry device information from common block
common printon_information, info
if (n_elements(info) eq 0) then begin
  message, 'PRINTON was not called prior to PRINTOFF: ' + $
    'nothing done', /continue
  return
endif

;- Close Printer device
device, /close_document

;- Switch to entry graphics device
set_plot, info.device

;- Restore window and font
if (info.window ge 0) then wset, info.window
!p.font = info.font

;- Report to user
if (keyword_set(quiet) eq 0) then $
  print, 'Ended PRINTER output'

END
