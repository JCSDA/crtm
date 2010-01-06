;+
; Function to read a simple XY binfile format.
;
; File format is
;   N
;   x1   yr1   [yi1]
;   x2   yr2   [yi2]  
;   x3   yr3   [yi3]
;   ......
;   xN   yrN   [yiN]
;
; Use the /COMPLEX keyword to read the 3-column format.
;
FUNCTION Read_XY_BinFile, fileName, $
                          COMPLEX=Complex, $
                          _EXTRA=Extra
;-
  ; Open the file
  fileId=Open_Binary_File(fileName,_EXTRA=Extra)

  ; Read the dataset
  n=0L
  READU,fileId,n
  x=DBLARR(n)
  y=DBLARR(n)
  READU,fileId,x
  READU,fileId,y
  IF (KEYWORD_SET(Complex)) THEN BEGIN
    iy=DBLARR(n)
    READU,fileId,iy
    data={n:n,x:TEMPORARY(x),ry:TEMPORARY(y),iy:TEMPORARY(iy)}
  ENDIF ELSE $
    data={n:n,x:TEMPORARY(x),y:TEMPORARY(y)}

  ; Close the file
  FREE_LUN, fileId

  ; Return the dataset
  RETURN, data

END
