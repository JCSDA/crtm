; docformat = 'rst'

;+
; Page the contents of the filename to the screen. 
;
; :Params:
;    filename : in, required, type=string
;       filename to display
;-
pro more, filename
  compile_opt strictarr
  
  nlines = file_lines(filename)
  output = strarr(1, nlines)
  openr, lun, filename, /get_lun
  readf, lun, output
  free_lun, lun
  
  terminal = !version.os_family eq 'unix' ? '/dev/tty' : 'CON:'
  openw, outlun, terminal, /get_lun, /more
  printf, outlun, output
  free_lun, outlun
end
