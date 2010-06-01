function mas_unpack, array

;+
; Purpose:
;   To unpack 12-bit MAS video data (Exabyte format) into a longword array.
;   The packed format stores 716 12-bit words in 1074 bytes.
;
; Input:
;   array        BYTE array with 1074 elements (716 12-bit words)
;
; Output:
;   mas_unpack   LONG array with 716 elements (unpacked video data)
;
; Notes:
;   This procedure is optimized for speed.
;
;-

if n_elements( array ) ne 1074 then $
  message, 'Input array must have 1074 elements'
  
packed = reform( array, 3, 358 )
out = intarr( 2, 358 )

out( 0, * ) = ishft( fix( packed( 0, * ) ), 4 ) + ishft( packed( 1, * ), -4 )
out( 1, * ) = ishft( packed( 1, * ) and 15, 8 ) + packed( 2, * )

return, long( reform( temporary( out ), 716 ) )

end
