;sensor='crisB3_npp'
sensor='iasiB3_metop-a'

file=sensor + '_fittingErr.txt'
openr, 10, file
readf, 10, n_channels
line=''
readf, 10, line
data_line=fltarr(8)
data=fltarr(n_channels, 8)
for i=0, n_channels-1 do begin
  readf, 10, data_line
  data[i, *] = data_line
endfor
close, 10

wn=data[*, 0]
plot, wn, data[*, 4]
;oplot, wn, data[*,4]
end 

