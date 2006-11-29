;+
; NAME:
;       Open_Binary_File
;
; PURPOSE:
;       Function to open the unformatted, sequential access
;       "Binary" files.
;
; CALLING SEQUENCE:
;       fileId = Open_Binary_File( fileName                       , $
;                                  WRITE          =Write          , $
;                                  SWAP_ENDIAN    =Swap_Endian    , $
;                                  F77_UNFORMATTED=F77_Unformatted  )
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
;       Swap_Endian:      Set this keyword parameter to byte swap data written
;                         to the file opened by this function. This keyword is
;                         ignored unless the WRITE keyword is also set.
;                         If NOT SET, data subsequently written to a file is written
;                                     in the native platform format.
;                            SET,     data subsequently written to a file is byte
;                                     swapped compared to the native platform format.
;                         UNITS:      N/A
;                         TYPE:       INTEGER
;                         DIMENSION:  Scalar
;                         ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       F77_Unformatted:  Set this keyword parameter to open a file equivalent to
;                         Fortran SEQUENTIAL, UNFORMATTED access.
;                         If NOT SET, data subsequently read from or written to a
;                                     file is treated as a "regular" byte stream.
;                                     (DEFAULT).
;                            SET,     the file format is such that each data record
;                                     is bounded by 4-byte record markers that contain
;                                     the size of the record.
;                         UNITS:      N/A
;                         TYPE:       INTEGER
;                         DIMENSION:  Scalar
;                         ATTRIBUTES: INTENT(IN), OPTIONAL
;
; FUNCTION RESULT:
;       FileID:           File unit number used for subsequent file access.
;                         UNITS:      N/A
;                         TYPE:       INTEGER
;                         DIMENSION:  Scalar
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, CIMSS/SSEC, 29-Nov-2006
;                       paul.vandelst@ssec.wisc.edu
;
;-

FUNCTION Open_Binary_File, fileName, $
                           WRITE          =Write, $
                           SWAP_ENDIAN    =Swap_Endian, $   ; Only for output
                           F77_UNFORMATTED=F77_Unformatted

  ; Define parameters
  MAGIC_NUMBER = 123456789L

  ; Define error handler
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    IF ( N_ELEMENTS(lun) NE 0 ) THEN FREE_LUN, lun
    RETURN, FAILURE
  ENDIF

  ; Get a free unit number
  GET_LUN, lun

  ; Check input
  IF ( NOT Valid_String(fileName) ) THEN $
    MESSAGE, 'Must specify a filename.', /NONAME, /NOPRINT

  ; Default is to open for read...
  Input=TRUE
  ; ...unless WRITE keyword is set.
  IF ( KEYWORD_SET(Write) ) THEN Input=FALSE
  
  IF ( Input ) THEN BEGIN
    ; Open the file for reading
    OPENR, lun, fileName, $
                F77_UNFORMATTED=F77_Unformatted
    ; Read the magic number
    mnRead = 0L
    READU, lun, mnRead
    ; Check it
    IF ( mnRead EQ MAGIC_NUMBER ) THEN RETURN, lun
    ; Check the swapped version
    IF ( SWAP_ENDIAN(mnRead) EQ MAGIC_NUMBER ) THEN BEGIN
      CLOSE, lun
      OPENR, lun, fileName, $
                  F77_UNFORMATTED=F77_Unformatted, $
                  /SWAP_ENDIAN
      READU, lun, mnRead
      RETURN, lun
    ENDIF
    ; Not a recognised BinFile
    MESSAGE, 'Unrecognised file type.', $
             /NONAME, /NOPRINT
  ENDIF ELSE BEGIN
    OPENW, lun, fileName, $
                SWAP_Endian    =Swap_Endian, $
                F77_UNFORMATTED=F77_Unformatted
    ; Write the magic number
    WRITEU, lun, MAGIC_NUMBER
    RETURN, lun
  ENDELSE

END ; FUNCTION Open_BinFile
