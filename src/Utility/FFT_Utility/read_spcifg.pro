PRO Read_SPCIFG, fileName, $
                 f, rSpc, iSpc, $
                 x, rIfg, iIfg

  fileId=Open_Binary_File(fileName,/F77_UNFORMATTED)

  n=0L
  READU,fileId,n
  f   =DBLARR(n)
  rSpc=DBLARR(n)
  iSpc=DBLARR(n)
  READU,fileId,f
  READU,fileId,rSpc
  READU,fileId,iSpc

  n2=0L
  READU,fileId,n2
  x   =DBLARR(n2)
  rIfg=DBLARR(n2)
  iIfg=DBLARR(n2)
  READU,fileId,x
  READU,fileId,rIfg
  READU,fileId,iIfg

  FREE_LUN, fileId

END
