PRO PSOFF, QUIET=QUIET

;- Check that PostScript output is active
if (!d.name ne 'PS') then begin
  message, 'POSTSCRIPT output not active: ' + $
    'nothing done', /continue
  return
endif

;- Get entry device information from common block
common pson_information, info
if (n_elements(info) eq 0) then begin
  message, 'PSON was not called prior to PSOFF: ' + $
    'nothing done', /continue
  return
endif

;- Close PostScript device
device, /close_file

;- Switch to entry graphics device
set_plot, info.device

;- Restore window and font
if (info.window ge 0) then wset, info.window
!p.font = info.font

;- Report to user
if (keyword_set(quiet) eq 0) then $
  print, info.filename, $
    format='("Ended POSTSCRIPT output to ", a)'

END
