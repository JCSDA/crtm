PRO read_pt_file, pt_file, nlev, alt_pt, p_pt, t_pt

; 02-OCT-09 Original. Vivienne Payne, AER Inc.
; 
; Description:
; ------------
; Routine to read in Manuel's pressure/temperature profiles for each 
; of the 48 UMBC atmospheres
; 
; Input:
; --------
; pt_file : string containing filename of pressure/temperature file
; 
; Output:
; --------
; nlev                     : number of vertical levels
; alt_pt(nlev)             : altitude grid (km)
; p_pt(nlev)               : pressure grid (hPa)
; t_pt(nlev)               ; kinetic temperature profile (K)
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

openr, lunfil, pt_file, /get_lun
header =  ' '
FOR i=0, 2 DO readf, lunfil, header
nlev =  0
readf, lunfil, nlev
FOR i=0, 2 DO readf, lunfil, header
alt_pt =  fltarr(nlev)
readf, lunfil, alt_pt
FOR i=0, 2 DO readf, lunfil, header
p_pt =  fltarr(nlev)
readf, lunfil, p_pt
FOR i=0, 2 DO readf, lunfil, header
t_pt =  fltarr(nlev)
readf, lunfil, t_pt
free_lun, lunfil

END

