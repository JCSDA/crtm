pro get_his_time, file, hour, minute, second

;+
; Purpose:
;   To get the time (hours) for each record in a HIS data file
;
; Input:
;   file    Name of the HIS file to read
;
; Output:
;   time    Time for each HIS record (hours)
;
; Note:
;   This routine will detect whether the data needs to be swapped from
;   big-endian format. Portions of this code originally from Paul van Delst.
;
; Liam Gumley, CIMSS/SSEC, 28-MAY-1996
;-

;- open file and get number of records

openr, lun_his, file, /get_lun
file_info = fstat( lun_his )
n_recs = file_info.size / ( 2150 * 4 )

;- define structure of data file

n_pts = 2049
record_length_words = 2150
record_length_bytes = record_length_words * 4
his_record = his_declare_file_structure()
his_data = replicate( his_record, n_recs )

;- read data and close file

readu, lun_his, his_data
free_lun, lun_his

;- convert time (HHMMSS) to hours, swapping if necessary

time = temporary( his_data(*).current_time )
loc = where( time lt 0.0, count )
if count ge 1 then time = swap_endian( temporary( time ) )

;- extract seconds, minutes, hours

second = time mod 100.0
minute = ( ( time mod 10000.0 ) - second ) / 100.0
hour   = ( time - ( minute * 100.0 ) - second ) / 10000.0
undefine, time

end
