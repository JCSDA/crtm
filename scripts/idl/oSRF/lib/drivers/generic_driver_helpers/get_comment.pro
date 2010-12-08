PRO get_comment, $
  Comment_File, $ ; Input
  comment         ; Output
  
  ; Read the source comment global attribute file
  finfo = FILE_INFO(Comment_File)
  IF ( NOT finfo.EXISTS ) THEN $
    MESSAGE, "Comment file "+Comment_File+" not found.", $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ; ...Read the comment
  comment = ""
  line_comment = ""
  n_File_Lines = FILE_LINES(Comment_File) 
  OPENR, fid, Comment_File, /GET_LUN
  FOR n = 0, n_File_Lines - 1 DO BEGIN
    READF, fid, line_comment
    comment = comment+line_comment
  ENDFOR
  FREE_LUN, fid
  ; ...Append any response threshold info
  IF ( N_ELEMENTS(Response_Threshold) GT 0 ) THEN BEGIN
    comment = comment+"; Response threshold used = "+STRING(Response_Threshold,FORMAT='(f7.4)')
  ENDIF 
  
END
