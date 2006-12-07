FUNCTION Read_DataSet, fileId, NOIMAG=NoImag

  @error_codes

  readImag=TRUE
  IF ( KEYWORD_SET(NoImag) ) THEN readImag=FALSE

  n=0L
  READU,fileId,n
  x=DBLARR(n)
  y=DBLARR(n)
  READU,fileId,x
  READU,fileId,y
  dataset = {n:n,x:x,ry:y}
  IF ( readImag ) THEN BEGIN
    READU,fileId,y
    dataset = CREATE_STRUCT(dataset, 'iy', y)
  ENDIF

  RETURN, dataset

END


PRO Read_SPCIFG, fileName, $
                 d1, $
                 d2=d2, $
                 NOIMAG=NoImag

  ; Open the file  
  fileId=Open_Binary_File(fileName)

  ; Read the first dataset
  d1 = Read_Dataset(fileId, NOIMAG=NoImag)

  ; Read the second data set if required
  IF (ARG_PRESENT(d2)) THEN $
    d2 = Read_Dataset(fileId, NOIMAG=NoImag)
  
  ; Close the file
  FREE_LUN, fileId

END
