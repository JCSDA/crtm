fname='iasiB3_metop-a.NLTE.error.txt'

spawn, 'wc -l '+fname+' > tmp.txt'
openr, 10, 'tmp.txt'
readf, 10, n
close, 10
n=n-1

wn=fltarr(n)
rms=fltarr(n)
maxdiff=fltarr(n)

openr,10, fname

skip=''
readf, 10, skip
for i = 0, n-1 do begin
  readf, 10, x1, x2, x3, x4, x5
  wn[i] = x2
  rms[i] = x4
  maxdiff[i] = x5
endfor
close, 10

plot, wn, maxdiff, yrange=[-0.5, 0.5], linestyle=0
oplot, wn, rms, linestyle=1

end
