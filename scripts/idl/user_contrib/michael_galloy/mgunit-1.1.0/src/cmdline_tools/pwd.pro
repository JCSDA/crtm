; docformat = 'rst'

;+
; Prints the IDL's current directory to the output log.
;-
pro pwd
  compile_opt strictarr, hidden

  cd, current=currentDir
  print, currentDir
end
