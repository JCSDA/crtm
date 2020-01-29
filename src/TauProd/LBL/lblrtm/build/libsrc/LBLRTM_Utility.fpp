!
! LBLRTM_Utility
!
! Module containing some LBLRTM-related utility routines
!
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, 23-Jan-2000
!                     paul.vandelst@noaa.gov
!

MODULE LBLRTM_Utility

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds       , ONLY: Byte, &
                               FP, IP, DP => Double, &
                               N_FP_BYTES => n_Bytes_FP, &
                               N_IP_BYTES => n_Bytes_IP, &
                               N_DP_BYTES => n_Bytes_Double
  USE Message_Handler  , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE File_Utility     , ONLY: Get_Lun, File_Open
  USE LBLRTM_Parameters, ONLY: LBLRTM_FILE_OK   , LBLRTM_FILE_OK_MSG   , &
                               LBLRTM_FILE_EOF  , LBLRTM_FILE_EOF_MSG  , &
                               LBLRTM_FILE_EOL  , LBLRTM_FILE_EOL_MSG  , &
                               LBLRTM_FILE_UNDEF, LBLRTM_FILE_UNDEF_MSG
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Procedures
  PUBLIC :: LBLRTM_EoF_Message
  PUBLIC :: LBLRTM_n_Points
  PUBLIC :: LBLRTM_EoL_Write
  PUBLIC :: LBLRTM_File_Open
  PUBLIC :: LBLRTM_UtilityVersion


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: LBLRTM_Utility.fpp 35189 2013-12-28 17:23:23Z paul.vandelst@noaa.gov $'
  ! Default message string length
  INTEGER, PARAMETER :: ML = 512


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LBLRTM_EoF_Message
!
! PURPOSE:
!       Pure function to return a message describing the LBLRTM file EOF status.
!
! CALLING SEQUENCE:
!       msg = LBLRTM_EoF_Message( eof )
!
! INPUTS:
!       eof:       LBLRTM file EoF status specifier.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       msg:       Character string describing the LBLRTM file EoF status.
!                  UNITS:      N/A
!                  TYPE:       CHARACTER(*)
!                  DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  PURE FUNCTION LBLRTM_EoF_Message( eof ) RESULT( msg )
    ! Arguments
    INTEGER, INTENT(IN) :: eof
    ! Function result
    CHARACTER(ML) :: msg

    msg = 'LBLRTM file status:'
    SELECT CASE (eof)
      CASE(LBLRTM_FILE_OK   ); msg = TRIM(msg)//' '//LBLRTM_FILE_OK_MSG
      CASE(LBLRTM_FILE_EOF  ); msg = TRIM(msg)//' '//LBLRTM_FILE_EOF_MSG
      CASE(LBLRTM_FILE_EOL  ); msg = TRIM(msg)//' '//LBLRTM_FILE_EOL_MSG
      CASE DEFAULT           ; msg = TRIM(msg)//' '//LBLRTM_FILE_UNDEF_MSG
    END SELECT

  END FUNCTION LBLRTM_EoF_Message


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LBLRTM_n_Points
!
! PURPOSE:
!       Pure function to compute the number of points in an LBLRTM spectrum.
!
! CALLING SEQUENCE:
!       n_Points = LBLRTM_n_Points( f1, f2, df )
!
! INPUTS:
!       f1:        Beginning frequency of the spectral data.
!                  UNITS:      cm^-1
!                  TYPE:       REAL(DP)
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN)
!
!       f2:        Ending frequency of the spectral data.
!                  UNITS:      cm^-1
!                  TYPE:       REAL(DP)
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN)
!
!       df:        Frequency spacing of the spectral data.
!                  The value of the data type kind, FP, is can
!                  indicate either single or double precision
!                  depending on how the Type_Kinds.fpp module was
!                  preprocessed for compilation.
!                  UNITS:      cm^-1
!                  TYPE:       REAL(FP)
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       n_Points:  The return value is an integer containing the
!                  number of points in the spectrum.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Scalar
!
! PROCEDURE:
!       The number of points is calculated from the begin and end frequencies,
!       f1 and f2, and the frequency interval, df, by:
!
!                   ( f2 - f1        )
!         n = FLOOR (--------- + 1.5 )
!                   (   df           )
!
!:sdoc-:
!--------------------------------------------------------------------------------

  PURE FUNCTION LBLRTM_n_Points( &
    f1, &  ! Input
    f2, &  ! Input
    df) &  ! Input
  RESULT( n_Points )
    ! Arguments
    REAL(DP), INTENT(IN) :: f1
    REAL(DP), INTENT(IN) :: f2
    REAL(FP), INTENT(IN) :: df
    ! Function result
    INTEGER :: n_Points
    ! Local variables
    REAL(DP) :: rn_points

    ! The calculation is done in two steps since some compilers
    ! complain (i.e.they issue "CAUTION" messages) when a REAL
    ! division occurs in an expression being converted to INTEGER
    rn_points = 1.5_DP + ((f2 - f1) / REAL(df,DP))
    n_Points  = FLOOR(rn_points)

  END FUNCTION LBLRTM_n_Points


!--------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       LBLRTM_EoL_Write
!
! PURPOSE:
!       Function to write an end-of-layer (EoL) marker to an output
!       LBLRTM format file.
!
! CALLING SEQUENCE:
!       Error_Status = LBLRTM_EoL_Write( FileID )
!
! INPUTS:
!       FileId:        The unit number for the already open LBLRTM file.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the LBLRTM file EOL write was successful
!                        == FAILURE an unrecoverable error occurred
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       If an error occurs writing to the file, it is closed.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION LBLRTM_EoL_Write( fid ) RESULT( err_stat )
    ! Arguments
    INTEGER, INTENT(IN) :: fid
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'LBLRTM_Utility::EoL_Write'
    ! Local variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    INTEGER :: i, n

    ! Setup
    err_stat = SUCCESS
    ! ...Check file is open
    IF ( .NOT. File_Open( fid ) ) THEN
      msg = 'LBLRTM file is not open'
      CALL Write_Cleanup(); RETURN
    END IF

    ! Determine how many integer elements need to be written
    ! ...How many bytes required?
    n = ( 2 * N_DP_BYTES ) + &  ! == Phdr begin and end frequency
        N_FP_BYTES         + &  ! == Phdr frequency interval
        N_IP_BYTES              ! == Phdr number of points
    ! ...Convert the number of bytes to the number of integers
    n = n / N_IP_BYTES

    ! Write the EoL marker
    WRITE(fid,IOSTAT=io_stat,IOMSG=io_msg ) [(INT(LBLRTM_FILE_EOL,IP), i=1,n)]
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing EOL marker - '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF

  CONTAINS

    SUBROUTINE Write_Cleanup()
      IF ( File_Open(fid) ) THEN
        CLOSE( fid,IOSTAT=io_stat,IOMSG=io_msg )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error closing output file during error cleanup - '//TRIM(io_msg)
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Write_Cleanup

  END FUNCTION LBLRTM_EoL_Write


!--------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       LBLRTM_File_Open
!
! PURPOSE:
!       Function to open an LBLRTM format data file. If the file is opened for
!       reading, then a check is performed to determine if the file is of the
!       right byte-sex.
!
! CALLING SEQUENCE:
!       Error_Status = LBLRTM_File_Open( &
!                        Filename, &
!                        FileId  , &
!                        For_Output = For_Output, &
!                        Quiet      = Quiet       )
!
! INPUTS:
!       FileName:      The LBLRTM formay datafile to open.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       FileId:        The unit number for file access.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       For_Output:    Set this logical argument to open a new file for
!                      writing. Default action is to open an existing file
!                      for read access.
!                      If == .FALSE., existing file is opened for READ access [DEFAULT]
!                                     ACTION='READ', STATUS='OLD'
!                         == .TRUE. , new file is opened for WRITE access.
!                                     ACTION='WRITE', STATUS='REPLACE'
!                      If not specified, the default is .FALSE.
!                      NOTE: If the file already exists and it is opened with
!                            this keyword set to .TRUE., the file is OVERWRITTEN.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Quiet:         Set this logical argument to suppress INFORMATION
!                      messages being printed to stdout
!                      If == .FALSE., INFORMATION messages are OUTPUT [DEFAULT].
!                         == .TRUE.,  INFORMATION messages are SUPPRESSED.
!                      If not specified, default is .FALSE.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the LBLRTM file open was successful
!                        == FAILURE an unrecoverable error occurred
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION LBLRTM_File_Open( &
    Filename  , &  ! Input
    FileId    , &  ! Output
    For_Output, &  ! Optional input
    Quiet     , &  ! Optional input
    Debug     ) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*)     , INTENT(IN)  :: Filename
    INTEGER          , INTENT(OUT) :: FileId
    LOGICAL, OPTIONAL, INTENT(IN)  :: For_Output
    LOGICAL, OPTIONAL , INTENT(IN) :: Quiet
    LOGICAL, OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'LBLRTM_Utility::File_Open'
    ! ...Define the byte offset into the file header
    ! ...for the Average_Layer_Temperature component
#ifndef INT_SIZE
#define INT_SIZE 4  /* Default 4-byte Long */
#endif
#ifndef REAL_SIZE
#define REAL_SIZE 8  /* Default 8-byte Double */
#endif
#if (INT_SIZE==8 && REAL_SIZE==8)
    INTEGER, PARAMETER :: OFFSET = 4 + 96
#elif (INT_SIZE==4 && REAL_SIZE==8)
    INTEGER, PARAMETER :: OFFSET = 0
#else
    INTEGER, PARAMETER :: OFFSET = 4 + 92
#endif
    ! Local variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    CHARACTER(7)  :: status, action
    INTEGER :: io_stat
    LOGICAL :: input
    LOGICAL :: noisy
    LOGICAL :: debug_output
    INTEGER :: fid
    INTEGER(Byte) :: dummy(OFFSET)
    REAL(FP) :: average_layer_temperature

    ! Setup
    err_stat = SUCCESS
    ! ...Read or write file?
    input = .TRUE.
    IF ( PRESENT(For_Output) ) input = .NOT. For_Output
    ! ...Set info output status
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Set debug option
    debug_output = .FALSE.
    IF ( PRESENT(debug) ) debug_output = debug
    IF ( debug_output ) THEN
      CALL Display_Message(ROUTINE_NAME,'Entering...',INFORMATION)
      noisy = .TRUE.
    END IF


    ! Check if compilation environment is set correctly
    SELECT CASE(OFFSET)
      CASE(100); msg = 'Set for DOUBLE-precision LBLRTM files'
      CASE( 96); msg = 'Set for SINGLE-precision LBLRTM files'
      CASE DEFAULT
        msg = 'Compile for 4-byte default integers and 8-byte default '//&
              'reals not valid for LBLRTM output files'
        CALL Open_CleanUp(); RETURN
    END SELECT
    IF ( noisy ) CALL Display_Message( ROUTINE_NAME,msg,INFORMATION )


    ! Branch depending on type of file I/O
    IF ( input ) THEN

      ! Determine if data needs to be byte-swapped
      ! ...Get a unit number
      fid = Get_Lun()
      IF ( fid < 0 ) THEN
        msg = 'Could not get a valid unit number for endian test'
        CALL Open_CleanUp(); RETURN
      END IF
      ! ...Open file as unformatted stream access
      OPEN(fid,FILE   = Filename     , &
               STATUS = 'OLD'        , &
               ACCESS = 'STREAM'     , &
               FORM   = 'UNFORMATTED', &
               ACTION = 'READ'       , &
               IOSTAT = io_stat      , &
               IOMSG  = io_msg         )
      IF ( io_stat /= 0 ) THEN
        msg = 'Error opening LBLRTM file '//TRIM(Filename)//' for endian test - '//TRIM(io_msg)
        CALL Open_CleanUp(); RETURN
      END IF
      ! ...Read data up to the Average_Layer_Temperature file header component
      READ(fid) dummy
      ! ...Read the Average_Layer_Temperature
      READ(fid) average_layer_temperature
      ! ...Check the value for reasonable values
      IF ( average_layer_temperature < 50.0_FP .OR. &
           average_layer_temperature > 400.0_FP ) THEN
        msg = 'Data needs to be byte-swapped!'
        CALL Open_CleanUp(); RETURN
      END IF
      ! ...Close file for "proper" open
      CLOSE(fid)

      ! Set the sepcifiers for reading
      status = 'OLD'
      action = 'READ'

    ELSE

      ! Set the sepcifiers for writing
      status = 'REPLACE'
      action = 'WRITE'

    END IF


    ! Open the file for I/O
    ! ...Get a unit number
    fid = Get_Lun()
    IF ( fid < 0 ) THEN
      msg = 'Could not get a valid unit number'
      CALL Open_CleanUp(); RETURN
    END IF
    ! ...Open file as unformatted sequential access
    OPEN(fid,FILE   = Filename     , &
             STATUS = status       , &
             ACCESS = 'SEQUENTIAL' , &
             FORM   = 'UNFORMATTED', &
             ACTION = action       , &
             IOSTAT = io_stat      , &
             IOMSG  = io_msg         )
    IF ( io_stat /= 0 ) THEN
      msg = 'Error opening LBLRTM file '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Open_CleanUp(); RETURN
    END IF


    ! All is good, so set the return result
    FileId = fid

  CONTAINS

    SUBROUTINE Open_CleanUp()
      IF ( File_Open(fid) ) THEN
        CLOSE(fid,IOSTAT=io_stat,IOMSG=io_msg)
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error closing file during error cleanup - '//TRIM(io_msg)
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Open_CleanUp

  END FUNCTION LBLRTM_File_Open


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LBLRTM_UtilityVersion
!
! PURPOSE:
!       Subroutine to return the version information for the module.
!
! CALLING SEQUENCE:
!       CALL LBLRTM_UtilityVersion( Id )
!
! OUTPUTS:
!       Id:     Character string containing the version Id information for the
!               module.
!               UNITS:      N/A
!               TYPE:       CHARACTER(*)
!               DIMENSION:  Scalar
!               ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE LBLRTM_UtilityVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE LBLRTM_UtilityVersion

END MODULE LBLRTM_Utility
