;+
; Builds the mgunit sav file. A typo on line 78 of parse_url.pro (IDL 6.4, 7.0)
; must be fixed for this build process to work.
;-

; clear any other compilations
.reset

; compile required code
.compile assert      
.compile mgutclirunner__define
.compile mguttestsuite__define
.compile mguthtmlrunner__define  
.compile mgutxmlrunner__define 
.compile mgutjunitrunner__define    
.compile mguttestcase__define
.compile mgunit
.compile mguttestrunner__define
.compile mgutcompoundrunner__define.pro
.compile mg_ansicode
.compile mg_src_root
.compile mg_options__define


; compile any system routines that are used in the required code
resolve_all

; create the sav file
save, filename='mgunit.sav', /routines, $
      description='MGunit unit testing framework'

exit
