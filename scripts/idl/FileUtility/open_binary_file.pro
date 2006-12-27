;+
; NAME:
;       Open_Binary_File
;
; PURPOSE:
;       Function to open the unformatted, sequential access
;       "Binary" files.
;
; CALLING SEQUENCE:
;       fileId = Open_Binary_File( fileName     , $
;                                  WRITE =Write , $
;                                  SWAP  =Swap  , $
;                                  STREAM=STREAM  )
;
; INPUTS:
;       fileName:         Name of the Binary file to open.
;                         UNITS:      N/A
;                         TYPE:       CHARACTER(*)
;                         DIMENSION:  Scalar
;                         ATTRIBUTES: INTENT(IN)
;
; INPUT KEYWORD PARAMETERS:
;       Write:            Set this keyword parameter to open a new file for
;                         writing. Default action is to open an existing file
;                         for read access. Note, if the file already exists and
;                         it is opened with this keyword set, the file is
;                         overwritten.
;                         If NOT SET, existing file is opened for READ access (DEFAULT)
;                                     ACTION='READ', STATUS='OLD'
;                            SET,     new file is opened for WRITE access.
;                                     ACTION='WRITE', STATUS='REPLACE'
;                         UNITS:      N/A
;                         TYPE:       INTEGER
;                         DIMENSION:  Scalar
;                         ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Swap:             Set this keyword parameter to byte swap data written
;                         to the file opened by this function.
;                         This keyword is ignored unless the WRITE keyword is
;                         also set.
;                         If NOT SET, data subsequently written to a file is written
;                                     in the native platform endian format.
;                            SET,     data subsequently written to a file is byte
;                                     swapped compared to the native platform byte
;                                     endian format.
;                         UNITS:      N/A
;                         TYPE:       INTEGER
;                         DIMENSION:  Scalar
;                         ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Stream:           Set this keyword parameter to set file access to an
;                         unformatted byte stream.
;                         If NOT SET, data access is Fortran SEQUENTIAL,
;                                     UNFORMATTED. (DEFAULT)
;                                     Each data record is bounded by 4-byte
;                                     record markers that contain the record size.
;                            SET,     data access is a "regular" byte stream with
;                                     no record markers.
;                         UNITS:      N/A
;                         TYPE:       INTEGER
;                         DIMENSION:  Scalar
;                         ATTRIBUTES: INTENT(IN), OPTIONAL
;
; FUNCTION RESULT:
;       fileId:           File unit number used for subsequent file access.
;                         UNITS:      N/A
;                         TYPE:       INTEGER
;                         DIMENSION:  Scalar
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, CIMSS/SSEC, 29-Nov-2006
;                       paul.vandelst@ssec.wisc.edu
;
;-

FUNCTION Check_Binary_File, fileName, $
                            STREAM=Stream, $
                            DEBUG =Debug

  ; Include the parameters file
  @binary_file_parameters

  ; Define error handler
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    IF ( N_ELEMENTS(fileId) NE 0 ) THEN FREE_LUN, fileId
    RETURN, CHECK_FAILURE
  ENDIF

  ; Define variables to read the record markers
  ; (if required) and the magic number
  r1 = 0L & mN = 0L & r2 = 0L

  ; Read the magic number
  OPENR, fileId, fileName, /GET_LUN
  IF ( KEYWORD_SET(Stream) ) THEN BEGIN
    READU, fileId, mN
  ENDIF ELSE BEGIN
    READU, fileId, r1, mN, r2
  ENDELSE
  FREE_LUN, fileId

  ; Check the native version
  IF ( mN EQ MAGIC_NUMBER ) THEN BEGIN
    IF ( KEYWORD_SET(Debug) ) THEN $
      MESSAGE, 'File in native endian format. No swapping.', /INFORMATIONAL
    RETURN, CHECK_NOSWAP
  ENDIF
  
  ; Check the swapped version
  IF ( SWAP_ENDIAN(mN) EQ MAGIC_NUMBER ) THEN BEGIN
    IF ( KEYWORD_SET(Debug) ) THEN $
      MESSAGE, 'File not native endian format. Byte swapping set.', /INFORMATIONAL
    RETURN, CHECK_SWAP
  ENDIF

  ; Not a recognised BinFile
  MESSAGE, 'Unrecognised file type. Invalid magic number', $
           /NONAME, /NOPRINT

END


FUNCTION Open_Binary_File, fileName, $
                           STREAM=Stream, $
                           WRITE =Write , $
                           SWAP  =Swap  , $  ; Only for output
                           DEBUG =Debug

  ; Include the parameters file
  @binary_file_parameters

  ; Define error handler
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    IF ( N_ELEMENTS(fileId) NE 0 ) THEN FREE_LUN, fileId
    RETURN, FAILURE
  ENDIF

  ; Check fileName
  IF ( NOT Valid_String(fileName) ) THEN $
    MESSAGE, 'Must specify a filename.', /NONAME, /NOPRINT

  ; Default access is sequential, unformatted...
  seqUnfmt=TRUE
  ; ... unless STREAM keyword is set
  IF ( KEYWORD_SET(Stream) ) THEN seqUnfmt=FALSE
  
  ; Default is to open for read...
  input=TRUE
  ; ...unless WRITE keyword is set.
  IF ( KEYWORD_SET(Write) ) THEN input=FALSE

  
  ; -------------------------
  ; Open the file accordingly
  ; -------------------------
  IF ( Input ) THEN BEGIN
    ; Check the file
    swapFlag=Check_Binary_File(fileName, STREAM=Stream, DEBUG=Debug)
    IF ( swapFlag EQ CHECK_FAILURE ) THEN $
      MESSAGE, 'Binary file check failed', /NONAME, /NOPRINT
    ; Open the file for reading
    OPENR, fileId, fileName, /GET_LUN, $
                   F77_UNFORMATTED=seqUnfmt, $
                   SWAP_ENDIAN    =swapFlag
    ; Read past the magic number
    mN = 0L
    READU, fileId, mN

  ENDIF ELSE BEGIN
    ; Open the file for writing
    OPENW, fileId, fileName, /GET_LUN, $
                   SWAP_ENDIAN    =Swap, $
                   F77_UNFORMATTED=SeqUnFmt
    ; Write the magic number
    WRITEU, fileId, MAGIC_NUMBER

  ENDELSE

  RETURN, fileId

END ; FUNCTION Open_BinFile
