pro int_time, time, hour, minute, second, fraction

;+
; Purpose:
;   Convert time in hours to hours, minutes, seconds, fraction of a second
;
; Input:
;   time        time in hours
; 
; Output:
;   hour        number of whole hours
;   minute      number of whole minutes
;   second      number of whole seconds
;   fraction    fraction of second
;-

hour = fix( time )
temp = 60.0 * ( time mod 1.0 )
minute = fix( temp )
temp = 60.0 * ( temp mod 1.0 )
second = fix( temp )
fraction = temp mod 1.0

end
