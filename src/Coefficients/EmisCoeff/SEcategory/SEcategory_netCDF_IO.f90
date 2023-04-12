!
! SEcategory_netCDF_IO
!
! Module containing routines to read and write SEcategory netCDF
! format files.
!
!
! CREATION HISTORY:
!
!       Written by: Cheng Dang, 12-Feb-2022
!                   dangch@ucar.edu

MODULE SEcategory_netCDF_IO

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds         , ONLY: fp, Double, Long
  USE Message_Handler    , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE File_Utility       , ONLY: File_Exists
  USE String_Utility     , ONLY: StrClean
  USE SEcategory_Define  , ONLY: SEcategory_type         , &
                                 SEcategory_Associated   , &
                                 SEcategory_Destroy      , &
                                 SEcategory_Create       , &
                                 SEcategory_Inspect      , &
                                 SEcategory_ValidRelease , &
                                 SEcategory_Info         , &
                                 SEcategory_Name         , &
                                 SEcategory_Index
  USE netcdf
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Procedures
  PUBLIC :: SEcategory_netCDF_InquireFile
  PUBLIC :: SEcategory_netCDF_ReadFile
  PUBLIC :: SEcategory_netCDF_WriteFile

  ! -----------------
  ! Module parameters
  ! -----------------
  ! Default msg string length
  INTEGER, PARAMETER :: ML = 1024
  ! Literal constants
  REAL(fp), PARAMETER :: FILL_FLOAT = -999.0_fp
  INTEGER , PARAMETER :: INT_ZERO = 0
  INTEGER , PARAMETER :: INT_ONE  = 1

  ! Global attribute names. Case sensitive
  CHARACTER(*), PARAMETER :: RELEASE_GATTNAME     = 'Release'
  CHARACTER(*), PARAMETER :: VERSION_GATTNAME     = 'Version'
  CHARACTER(*), PARAMETER :: DATA_SOURCE_GATTNAME = 'Data_Source'
  CHARACTER(*), PARAMETER :: TITLE_GATTNAME       = 'Title'
  CHARACTER(*), PARAMETER :: HISTORY_GATTNAME     = 'History'
  CHARACTER(*), PARAMETER :: COMMENT_GATTNAME     = 'Comment'
  CHARACTER(*), PARAMETER :: DATATYPE_NAME_GATTNAME       = 'Datatype_Name'
  CHARACTER(*), PARAMETER :: CLASSIFICATION_NAME_GATTNAME = 'Classification_Name'


  ! Dimension names
  CHARACTER(*), PARAMETER :: TNSL_DIMNAME       = 'String_Length'
  CHARACTER(*), PARAMETER :: FREQUENCY_DIMNAME  = 'n_Frequencies'
  CHARACTER(*), PARAMETER :: TYPE_DIMNAME       = 'n_Surface_Types'

  ! Variable names
  CHARACTER(*), PARAMETER :: REFLECTANCE_VARNAME  = 'Reflectance'
  CHARACTER(*), PARAMETER :: FREQUENCY_VARNAME    = 'Frequency'
  CHARACTER(*), PARAMETER :: TYPE_VARNAME         = 'Surface_Type'
  CHARACTER(*), PARAMETER :: TYPE_ISVALID_VARNAME = 'Surface_Type_IsValid'

  ! Variable long name attribute.
  CHARACTER(*), PARAMETER :: LONGNAME_ATTNAME = 'long_name'
  CHARACTER(*), PARAMETER :: REFLECTANCE_LONGNAME  = 'Reflectance'
  CHARACTER(*), PARAMETER :: FREQUENCY_LONGNAME    = 'Frequency'
  CHARACTER(*), PARAMETER :: TYPE_LONGNAME         = 'Surface Type'
  CHARACTER(*), PARAMETER :: TYPE_ISVALID_LONGNAME = 'Surface Type IsValid'

  ! Variable description attribute.
  CHARACTER(*), PARAMETER :: DESCRIPTION_ATTNAME = 'description'
  CHARACTER(*), PARAMETER :: REFLECTANCE_DESCRIPTION  = 'Reflectance'
  CHARACTER(*), PARAMETER :: FREQUENCY_DESCRIPTION    = 'Frequency'
  CHARACTER(*), PARAMETER :: TYPE_DESCRIPTION         = 'Surface Type'
  CHARACTER(*), PARAMETER :: TYPE_ISVALID_DESCRIPTION = 'Surface Type IsValid'

  ! Variable units attribute.
  CHARACTER(*), PARAMETER :: UNITS_ATTNAME = 'units'
  CHARACTER(*), PARAMETER :: REFLECTANCE_UNITS  = 'N/A'
  CHARACTER(*), PARAMETER :: FREQUENCY_UNITS    = 'cm^-1'
  CHARACTER(*), PARAMETER :: TYPE_UNITS         = 'N/A'
  CHARACTER(*), PARAMETER :: TYPE_ISVALID_UNITS = 'N/A'

  ! Variable _FillValue attribute.
  CHARACTER(*), PARAMETER :: FILLVALUE_ATTNAME = '_FillValue'
  REAL(Double), PARAMETER :: REFLECTANCE_FILLVALUE  = FILL_FLOAT
  REAL(Double), PARAMETER :: FREQUENCY_FILLVALUE    = FILL_FLOAT
  CHARACTER(*), PARAMETER :: TYPE_FILLVALUE         = NF90_FILL_CHAR
  INTEGER     , PARAMETER :: TYPE_ISVALID_FILLVALUE = INT_ZERO

  ! Variable types
  INTEGER, PARAMETER :: REFLECTANCE_TYPE   = NF90_DOUBLE
  INTEGER, PARAMETER :: FREQUENCY_TYPE     = NF90_DOUBLE
  INTEGER, PARAMETER :: TYPE_TYPE          = NF90_CHAR
  INTEGER, PARAMETER :: TYPE_ISVALID_TYPE  = NF90_INT

CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################
!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SEcategory_netCDF_InquireFile
!
! PURPOSE:
!       Function to inquire SEcategory object files.
!
! CALLING SEQUENCE:
!       Error_Status = SEcategory_netCDF_InquireFile( &
!                        Filename, &
!                        netCDF           = netCDF          , &
!                        n_Frequencies    = n_Frequencies   , &
!                        n_Surface_Types  = n_Surface_Types , &
!                        Release          = Release         , &
!                        Version          = Version         , &
!                        Title            = Title           , &
!                        History          = History         , &
!                        Comment          = Comment           )
!
! INPUTS:
!       Filename:          Character string specifying the name of a
!                          SEcategory data file to read.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       netCDF:            Set this logical argument to access netCDF format
!                          SEcategory datafiles.
!                          If == .FALSE., file format is BINARY [DEFAULT].
!                             == .TRUE.,  file format is NETCDF.
!                          If not specified, default is .FALSE.
!                          UNITS:      N/A
!                          TYPE:       LOGICAL
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUTS:
!       n_Frequencies:     The number of frequencies in the LUT.
!                          Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       n_Surface_Types:   The number of surface types in
!                          the LUT. Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       Release:           The release number of the SEcategory file.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:           The version number of the SEcategory file.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Title:             Character string written into the TITLE global
!                          attribute field of the SEcategory file.
!                          This argument is ignored if the netCDF argument
!                          is not supplied or set.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:           Character string written into the HISTORY global
!                          attribute field of the SEcategory file.
!                          This argument is ignored if the netCDF argument
!                          is not supplied or set.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:           Character string written into the COMMENT global
!                          attribute field of the SEcategory file.
!                          This argument is ignored if the netCDF argument
!                          is not supplied or set.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
! FUNCTION RESULT:
!       Error_Status:      The return value is an integer defining the error status.
!                          The error codes are defined in the Message_Handler module.
!                          If == SUCCESS, the file inquire was successful
!                             == FAILURE, an unrecoverable error occurred.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------
  FUNCTION SEcategory_netCDF_InquireFile( &
    Filename        , &  ! Input
    n_Frequencies   , &  ! Optional output
    n_Surface_Types , &  ! Optional output
    Release         , &  ! Optional output
    Version         , &  ! Optional output
    Title           , &  ! Optional output
    History         , &  ! Optional output
    Comment         ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Frequencies
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Surface_Types
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release
    INTEGER     , OPTIONAL, INTENT(OUT) :: Version
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'SEcategory_netCDF_InquireFile'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: Close_File
    INTEGER :: NF90_Status
    INTEGER :: FileId
    INTEGER :: DimId
    TYPE(SEcategory_type) :: SEcategory

    ! Set up
    err_stat = SUCCESS
    Close_File = .FALSE.

    ! Open the file
    NF90_Status = NF90_OPEN( Filename,NF90_NOWRITE,FileId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error opening '//TRIM(Filename)//' for read access - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    ! ...Close the file if any error from here on
    Close_File = .TRUE.

    ! Get the dimensions
    ! ...n_Frequencies dimension
    NF90_Status = NF90_INQ_DIMID( FileId,FREQUENCY_DIMNAME,DimId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//FREQUENCY_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_INQUIRE_DIMENSION( FileId,DimId,Len=SEcategory%n_Frequencies )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//FREQUENCY_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    ! ...n_Surface_Types dimension
    NF90_Status = NF90_INQ_DIMID( FileId,TYPE_DIMNAME,DimId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//TYPE_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_INQUIRE_DIMENSION( FileId,DimId,Len=SEcategory%n_Surface_Types )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//TYPE_DIMNAME//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF

    ! Get the global attributes
    err_stat = ReadGAtts( Filename, &
                          FileId  , &
                          Release = SEcategory%Release, &
                          Version = SEcategory%Version, &
                          Title   = Title  , &
                          History = History, &
                          Comment = Comment  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading global attributes from '//TRIM(Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF

    ! Close the file
    NF90_Status = NF90_CLOSE( FileId )
    Close_File = .FALSE.
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error closing input file - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF

    ! Set the return values
    IF ( PRESENT(n_Frequencies   ) ) n_Frequencies    = SEcategory%n_Frequencies
    IF ( PRESENT(n_Surface_Types ) ) n_Surface_Types  = SEcategory%n_Surface_Types
    IF ( PRESENT(Release         ) ) Release          = SEcategory%Release
    IF ( PRESENT(Version         ) ) Version          = SEcategory%Version

  CONTAINS

    SUBROUTINE Inquire_CleanUp()
      IF ( Close_File ) THEN
        NF90_Status = NF90_CLOSE( FileId )
        IF ( NF90_Status /= NF90_NOERR ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup.'
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Inquire_CleanUp

  END FUNCTION SEcategory_netCDF_InquireFile

!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SEcategory_netCDF_WriteFile
!
! PURPOSE:
!       Function to write SEcategory object files.
!
! CALLING SEQUENCE:
!       Error_Status = SEcategory_netCDF_WriteFile( &
!                        SEcategory, &
!                        Filename, &
!                        netCDF  = netCDF , &
!                        Quiet   = Quiet  , &
!                        Title   = Title  , &
!                        History = History, &
!                        Comment = Comment  )
!
! INPUTS:
!       SEcategory:   Object containing the IRwater coefficient data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(SEcategory_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Filename:       Character string specifying the name of the
!                       SEcategory data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       netCDF:         Set this logical argument to access netCDF format
!                       SEcategory datafiles.
!                       If == .FALSE., file format is BINARY [DEFAULT].
!                          == .TRUE.,  file format is NETCDF.
!                       If not specified, default is .FALSE.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       No_Close:       Set this logical argument to *NOT* close the datafile
!                       upon exiting this routine. This option is required if
!                       the SEcategory data is embedded within another file.
!                       If == .FALSE., File is closed upon function exit [DEFAULT].
!                          == .TRUE.,  File is NOT closed upon function exit
!                       If not specified, default is .FALSE.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Quiet:          Set this logical argument to suppress INFORMATION
!                       messages being printed to stdout
!                       If == .FALSE., INFORMATION messages are OUTPUT [DEFAULT].
!                          == .TRUE.,  INFORMATION messages are SUPPRESSED.
!                       If not specified, default is .FALSE.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Title:          Character string written into the TITLE global
!                       attribute field of the SEcategory file.
!                       This argument is ignored if the netCDF argument
!                       is not supplied or set.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:        Character string written into the HISTORY global
!                       attribute field of the SEcategory file.
!                       This argument is ignored if the netCDF argument
!                       is not supplied or set.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:        Character string written into the COMMENT global
!                       attribute field of the SEcategory file.
!                       This argument is ignored if the netCDF argument
!                       is not supplied or set.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS the data write was successful
!                          == FAILURE an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------
  FUNCTION SEcategory_netCDF_WriteFile( &
    SEcategory, &
    Filename  , &
    Quiet     , &
    Title     , &
    History   , &
    Comment   , &
    Debug     ) &
  RESULT( err_stat )
    ! Arguments
    TYPE(SEcategory_type),  INTENT(IN) :: SEcategory
    CHARACTER(*),           INTENT(IN) :: Filename
    LOGICAL,      OPTIONAL, INTENT(IN) :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
    LOGICAL,      OPTIONAL, INTENT(IN) :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'SEcategory_netCDF_WriteFile'
    ! Local variables
    CHARACTER(ML) :: msg
    LOGICAL :: Close_File
    LOGICAL :: Noisy
    INTEGER :: NF90_Status
    INTEGER :: FileId
    INTEGER :: VarId
    INTEGER :: i
    INTEGER :: AllocateStatus
    INTEGER, ALLOCATABLE :: INT_Surface_Type_IsValid(:)


    ! Set up
    err_stat = SUCCESS
    Close_File = .FALSE.
    ! ...Check structure pointer association status
    IF ( .NOT. SEcategory_Associated( SEcategory ) ) THEN
      msg = 'SEcategory structure is empty. Nothing to do!'
      CALL Write_CleanUp(); RETURN
    END IF
    ! ...Check if release is valid
    IF ( .NOT. SEcategory_ValidRelease( SEcategory ) ) THEN
      msg = 'SEcategory Release check failed.'
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Check Quiet argument
    Noisy = .TRUE.
    IF ( PRESENT(Quiet) ) Noisy = .NOT. Quiet

    ! Create the output file
    err_stat = CreateFile( &
                 Filename                                             , &  ! Input
                 SEcategory%n_Frequencies                             , &  ! Input
                 SEcategory%n_Surface_Types                           , &  ! Input
                 FileId                                               , &  ! Output
                 Version             = SEcategory%Version             , &  ! Optional input
                 Classification_Name = SEcategory%Classification_Name , &  ! Optional input, required for IR land
                 Title               = Title                          , &  ! Optional input
                 History             = History                        , &  ! Optional input
                 Comment             = Comment                          )  ! Optional input
    IF ( err_stat /= SUCCESS ) THEN
       msg = 'Error creating output file '//TRIM(Filename)
       CALL Write_Cleanup(); RETURN
    END IF

    ! ...Close the file if any error from here on
    Close_File = .TRUE.

    ! Write the data items
    ! ...Surface Type variable
    NF90_Status = NF90_INQ_VARID( FileId,TYPE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//TYPE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarId,SEcategory%Surface_Type )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//TYPE_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Frequency variable
    NF90_Status = NF90_INQ_VARID( FileId,FREQUENCY_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//FREQUENCY_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarId,SEcategory%Frequency )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//FREQUENCY_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Reflectance variable
    NF90_Status = NF90_INQ_VARID( FileId,REFLECTANCE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//REFLECTANCE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarId,SEcategory%Reflectance )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//REFLECTANCE_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    !...Surface Type IsValid variable
    !...Convert LOGICAL variable to INTEGER for netCDF I/O
    ALLOCATE(INT_Surface_Type_IsValid(SEcategory%n_Surface_Types))
    DO i = 1, SEcategory%n_Surface_Types
      IF (SEcategory%Surface_Type_IsValid(i)) THEN
        INT_Surface_Type_IsValid(i) = INT_ONE
      ELSE
        INT_Surface_Type_IsValid(i) = INT_ZERO
      END IF
    END DO
    NF90_Status = NF90_INQ_VARID( FileId,TYPE_ISVALID_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//TYPE_ISVALID_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarId,INT_Surface_Type_IsValid )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//TYPE_ISVALID_VARNAME//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    DEALLOCATE(INT_Surface_Type_IsValid)

    ! Close the file
    NF90_Status = NF90_CLOSE( FileId )
    Close_File = .FALSE.
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error closing output file - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF

    ! Output an info message
    IF ( Noisy ) THEN
      CALL SEcategory_Info( SEcategory, msg )
      CALL Display_Message( ROUTINE_NAME, 'FILE: '//TRIM(Filename)//'; '//TRIM(msg), INFORMATION )
    END IF

  CONTAINS

    SUBROUTINE Write_CleanUp()
      IF ( Close_File ) THEN
        NF90_Status = NF90_CLOSE( FileId )
        IF ( NF90_Status /= NF90_NOERR ) &
          msg = TRIM(msg)//'; Error closing output file during error cleanup - '//&
                TRIM(NF90_STRERROR( NF90_Status ))
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Write_CleanUp

  END FUNCTION SEcategory_netCDF_WriteFile

!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SEcategory_netCDF_ReadFile
!
! PURPOSE:
!       Function to read SEcategory object files.
!
! CALLING SEQUENCE:
!       Error_Status = SEcategory_netCDF_ReadFile( &
!                        SEcategory, &
!                        Filename, &
!                        Quiet   = Quiet  , &
!                        Title   = Title  , &
!                        History = History, &
!                        Comment = Comment  )
!
! INPUTS:
!       Filename:          Character string specifying the name of a
!                          SEcategory data file to read.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       SEcategory:   Object containing the IRwater coefficient data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(SEcategory_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       Quiet:          Set this logical argument to suppress INFORMATION
!                       messages being printed to stdout
!                       If == .FALSE., INFORMATION messages are OUTPUT [DEFAULT].
!                          == .TRUE.,  INFORMATION messages are SUPPRESSED.
!                       If not specified, default is .FALSE.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUTS:
!       Title:             Character string written into the TITLE global
!                          attribute field of the SEcategory file.
!                          This argument is ignored if the netCDF argument
!                          is not supplied or set.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:           Character string written into the HISTORY global
!                          attribute field of the SEcategory file.
!                          This argument is ignored if the netCDF argument
!                          is not supplied or set.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:           Character string written into the COMMENT global
!                          attribute field of the SEcategory file.
!                          This argument is ignored if the netCDF argument
!                          is not supplied or set.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
! FUNCTION RESULT:
!       Error_Status:      The return value is an integer defining the error status.
!                          The error codes are defined in the Message_Handler module.
!                          If == SUCCESS, the file inquire was successful
!                             == FAILURE, an unrecoverable error occurred.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------
  FUNCTION SEcategory_netCDF_ReadFile( &
    SEcategory    , &  ! Output
    Filename      , &  ! Input
    Quiet         , &  ! Optional input
    Title         , &  ! Optional output
    History       , &  ! Optional output
    Comment       , &  ! Optional output
    Debug     )     &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    TYPE(SEcategory_type),  INTENT(OUT) :: SEcategory
    CHARACTER(*),           INTENT(IN)  :: Filename
    LOGICAL,      OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    LOGICAL,      OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'SEcategory_netCDF_ReadFile'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: Close_File
    LOGICAL :: Noisy
    INTEGER :: NF90_Status
    INTEGER :: FileId
    INTEGER :: n_Frequencies
    INTEGER :: n_Surface_Types
    INTEGER :: VarId
    INTEGER :: AllocateStatus
    INTEGER :: i
    INTEGER, ALLOCATABLE :: INT_Surface_Type_IsValid(:)

    ! Set up
    err_stat = SUCCESS
    Close_File = .FALSE.
    ! ...Check that the file exists
    IF ( .NOT. File_Exists(Filename) ) THEN
      msg = 'File '//TRIM(Filename)//' not found.'
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Check Quiet argument
    Noisy = .TRUE.
    IF ( PRESENT(Quiet) ) Noisy = .NOT. Quiet

    ! Inquire the file to get the dimensions
    err_stat = SEcategory_netCDF_InquireFile( &
                 Filename                         , &
                 n_Frequencies   = n_Frequencies  , &
                 n_Surface_Types = n_Surface_Types  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error obtaining SEcategory dimensions from '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF

    ! Allocate the output structure
    CALL SEcategory_Create( &
           SEcategory                , &
           n_Frequencies  , &
           n_Surface_Types  )
    IF ( .NOT. SEcategory_Associated( SEcategory ) ) THEN
      msg = 'SEcategory object allocation failed.'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Open the file for reading
    NF90_Status = NF90_OPEN( Filename,NF90_NOWRITE,FileId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error opening '//TRIM(Filename)//' for read access - '//&
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Close the file if any error from here on
    Close_File = .TRUE.

    ! Read the global attributes
    err_stat = ReadGAtts( Filename, &
                          FileID  , &
                          Release                 = SEcategory%Release            , &
                          Version                 = SEcategory%Version            , &
                          Classification_Name     = SEcategory%Classification_Name, &
                          Title                   = Title                         , &
                          History                 = History                       , &
                          Comment                 = Comment                         )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading global attribute from '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Check if release is valid
    IF ( .NOT. SEcategory_ValidRelease( SEcategory ) ) THEN
      msg = 'SEcategory Release check failed.'
      CALL Read_Cleanup(); RETURN
    END IF

    ! Read the SEcategory data
    ! ...Surface Type variable
    NF90_Status = NF90_INQ_VARID( FileId,TYPE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//TYPE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( FileId,VarId,SEcategory%Surface_Type )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//TYPE_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Frequency variable
    NF90_Status = NF90_INQ_VARID( FileId,FREQUENCY_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//FREQUENCY_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( FileId,VarId,SEcategory%Frequency )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//FREQUENCY_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Reflectance variable
    NF90_Status = NF90_INQ_VARID( FileId,REFLECTANCE_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//REFLECTANCE_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( FileId,VarId,SEcategory%Reflectance )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//REFLECTANCE_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    !...Surface Type IsValid variable
    ALLOCATE(INT_Surface_Type_IsValid(SEcategory%n_Surface_Types))
    NF90_Status = NF90_INQ_VARID( FileId,TYPE_ISVALID_VARNAME,VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//TYPE_ISVALID_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_GET_VAR( FileId,VarId,INT_Surface_Type_IsValid )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading '//TYPE_ISVALID_VARNAME//' from '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF
    !...Convert INTEGER to LOGICAL variable to INTEGER for SEcategory
    DO i = 1, SEcategory%n_Surface_Types
      IF (INT_Surface_Type_IsValid(i) == INT_ONE ) THEN
        SEcategory%Surface_Type_IsValid(i) = .TRUE.
      ELSE
        SEcategory%Surface_Type_IsValid(i) = .FALSE.
      END IF
    END DO
    DEALLOCATE(INT_Surface_Type_IsValid)

    ! Close the file
    NF90_Status = NF90_CLOSE( FileId ); Close_File = .FALSE.
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error closing output file - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Read_Cleanup(); RETURN
    END IF

    ! Output an info message
    IF ( Noisy ) THEN
      CALL SEcategory_Info( SEcategory, msg )
      CALL Display_Message( ROUTINE_NAME, 'FILE: '//TRIM(Filename)//'; '//TRIM(msg), INFORMATION )
    END IF

  CONTAINS

    SUBROUTINE Read_CleanUp()
      IF ( Close_File ) THEN
        NF90_Status = NF90_CLOSE( FileId )
        IF ( NF90_Status /= NF90_NOERR ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup- '//&
                TRIM(NF90_STRERROR( NF90_Status ))
      END IF
      CALL SEcategory_Destroy( SEcategory )
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Read_CleanUp

  END FUNCTION SEcategory_netCDF_ReadFile


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

  ! Function to write the global attributes to a SEcategory data file.

  FUNCTION WriteGAtts( &
    Filename            , &  ! Input
    FileId              , &  ! Input
    Version             , &  ! Optional input
    Classification_Name , &  ! Optional input
    Title               , &  ! Optional input
    History             , &  ! Optional input
    Comment             ) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN) :: Filename
    INTEGER     ,           INTENT(IN) :: FileId
    INTEGER     , OPTIONAL, INTENT(IN) :: Version
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Classification_Name
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'SEcategory_WriteGAtts(netCDF)'
    CHARACTER(*), PARAMETER :: WRITE_MODULE_HISTORY_GATTNAME   = 'write_module_history'
    CHARACTER(*), PARAMETER :: CREATION_DATE_AND_TIME_GATTNAME = 'creation_date_and_time'
    ! Local variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: GAttName
    CHARACTER(8)  :: cdate
    CHARACTER(10) :: ctime
    CHARACTER(5)  :: czone
    CHARACTER(ML) :: CLSname
    INTEGER :: Ver
    INTEGER :: NF90_Status
    TYPE(SEcategory_type) :: SEcategory

    ! Set up
    err_stat = SUCCESS
    msg = ' '

    ! Mandatory global attributes
    ! ...Software ID
    !GAttName = WRITE_MODULE_HISTORY_GATTNAME
    !NF90_Status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(GAttName),MODULE_VERSION_ID )
    !IF ( NF90_Status /= NF90_NOERR ) THEN
    !  CALL WriteGAtts_Cleanup(); RETURN
    !END IF
    ! ...Creation date
    CALL DATE_AND_TIME( cdate, ctime, czone )
    GAttName = CREATION_DATE_AND_TIME_GATTNAME
    NF90_Status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(GAttName), &
                                cdate(1:4)//'/'//cdate(5:6)//'/'//cdate(7:8)//', '// &
                                ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//' '// &
                                czone//'UTC' )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF
    ! ...The Release
    GAttName = RELEASE_GATTNAME
    NF90_Status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(GAttName),SEcategory%Release )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF

    ! Optional global attributes
    ! ...The Version
    IF ( PRESENT(Version) ) THEN
      Ver = Version
    ELSE
      Ver = SEcategory%Version
    END IF
    GAttName = VERSION_GATTNAME
    NF90_Status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(GAttName),Ver )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF
    ! ...The Classification_Name, required for IR land
    IF ( PRESENT(Classification_Name) ) THEN
      CLSname = Classification_Name
    ELSE
      CLSname = SEcategory%Classification_Name
    END IF
    GAttName = CLASSIFICATION_NAME_GATTNAME
    NF90_Status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(GAttName),CLSname )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF
    ! ...The title
    IF ( PRESENT(title) ) THEN
      GAttName = TITLE_GATTNAME
      NF90_Status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(GAttName),title )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ...The history
    IF ( PRESENT(history) ) THEN
      GAttName = HISTORY_GATTNAME
      NF90_Status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(GAttName),history )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ...The comment
    IF ( PRESENT(comment) ) THEN
      GAttName = COMMENT_GATTNAME
      NF90_Status = NF90_PUT_ATT( FileId,NF90_GLOBAL,TRIM(GAttName),comment )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF

 CONTAINS

    SUBROUTINE WriteGAtts_CleanUp()
      NF90_Status = NF90_CLOSE( FileId )
      IF ( NF90_Status /= NF90_NOERR ) &
        msg = '; Error closing input file during error cleanup - '//&
              TRIM(NF90_STRERROR( NF90_Status ) )
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//TRIM(GAttName)//' attribute to '//&
                            TRIM(Filename)//' - '// &
                            TRIM(NF90_STRERROR( NF90_Status ) )//TRIM(msg), &
                            err_stat )
    END SUBROUTINE WriteGAtts_CleanUp

  END FUNCTION WriteGAtts

  ! Function to read the global attributes from a SEcategory data file.

  FUNCTION ReadGAtts(&
    Filename            , &  ! Input
    FileId              , &  ! Input
    Release             , &  ! Optional output
    Version             , &  ! Optional output
    Classification_Name , &  ! Optional output
    Title               , &  ! Optional output
    History             , &  ! Optional output
    Comment             ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     ,           INTENT(IN)  :: FileId
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release
    INTEGER     , OPTIONAL, INTENT(OUT) :: Version
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Classification_Name
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'SEcategory_ReadGAtts(netCDF)'
    ! Local variables
    CHARACTER(ML)   :: msg
    CHARACTER(256)  :: GAttName
    CHARACTER(5000) :: GAttString
    INTEGER :: NF90_Status

    ! Set up
    err_stat = SUCCESS

    ! The global attributes
    ! ...The Release
    IF ( PRESENT(Release) ) THEN
      GAttName = RELEASE_GATTNAME
      NF90_Status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(GAttName),Release )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ...The Version
    IF ( PRESENT(Version) ) THEN
      GAttName = VERSION_GATTNAME
      NF90_Status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(GAttName),Version )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
    END IF
    ! ...The Classification Name
    IF ( PRESENT(Classification_Name) ) THEN
      GAttName = CLASSIFICATION_NAME_GATTNAME; GAttString = ''
      NF90_Status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(GAttName),GAttString )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
      CALL StrClean( GAttString )
      Classification_Name = GAttString(1:MIN(LEN(Classification_Name), LEN_TRIM(GAttString)))
    END IF
    ! ...The title
    IF ( PRESENT(title) ) THEN
      GAttName = TITLE_GATTNAME; GAttString = ''
      NF90_Status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(GAttName),GAttString )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
      CALL StrClean( GAttString )
      title = GAttString(1:MIN(LEN(title), LEN_TRIM(GAttString)))
    END IF
    ! ...The history
    IF ( PRESENT(history) ) THEN
      GAttName = HISTORY_GATTNAME; GAttString = ''
      NF90_Status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(GAttName),GAttString )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
      CALL StrClean( GAttString )
      history = GAttString(1:MIN(LEN(history), LEN_TRIM(GAttString)))
    END IF
    ! ...The comment
    IF ( PRESENT(comment) ) THEN
      GAttName = COMMENT_GATTNAME; GAttString = ''
      NF90_Status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(GAttName),GAttString )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
      CALL StrClean( GAttString )
      comment = GAttString(1:MIN(LEN(comment), LEN_TRIM(GAttString)))
    END IF

  CONTAINS

    SUBROUTINE ReadGAtts_CleanUp()
      err_stat = FAILURE
      msg = 'Error reading '//TRIM(GAttName)//' attribute from '//TRIM(Filename)//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ) )
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE ReadGAtts_CleanUp

  END FUNCTION ReadGAtts

  FUNCTION CreateFile( &
    Filename            , &  ! Input
    n_Frequencies       , &  ! Input
    n_Surface_Types     , &  ! Input
    FileId              , &  ! Output
    Version             , &  ! Optional input
    Classification_Name , &  ! Optional input
    Title               , &  ! Optional input
    History             , &  ! Optional input
    Comment             ) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     ,           INTENT(IN)  :: n_Frequencies
    INTEGER     ,           INTENT(IN)  :: n_Surface_Types
    INTEGER     ,           INTENT(OUT) :: FileId
    INTEGER     , OPTIONAL, INTENT(IN)  :: Version
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Classification_Name
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: History
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'SEcategory_CreateFile(netCDF)'
    ! Local variables
    CHARACTER(ML) :: msg
    LOGICAL :: Close_File
    INTEGER :: NF90_Status
    INTEGER :: n_Frequencies_DimID
    INTEGER :: n_Surface_Types_DimID
    INTEGER :: tnsl_DimID
    INTEGER :: varID
    INTEGER :: Put_Status(4)
    TYPE(SEcategory_type) :: dummy

    ! Setup
    err_stat = SUCCESS
    Close_File = .FALSE.

    ! Create the data file
    NF90_Status = NF90_CREATE( Filename,NF90_CLOBBER,FileId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error creating '//TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Close the file if any error from here on
    Close_File = .TRUE.

    ! Define the dimensions
    ! ...Number of frequencies
    NF90_Status = NF90_DEF_DIM( FileID,FREQUENCY_DIMNAME,n_Frequencies,n_Frequencies_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//FREQUENCY_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Number of surface types
    NF90_Status = NF90_DEF_DIM( FileID,TYPE_DIMNAME,n_Surface_Types,n_Surface_Types_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//TYPE_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Type_Name string length
    CALL SEcategory_Create(dummy,0,1) ! Only n_Types dimension non-zero
    NF90_Status = NF90_DEF_DIM( FileID,TNSL_DIMNAME,LEN(dummy%Surface_Type(1)),tnsl_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//TNSL_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    CALL SEcategory_Destroy(dummy)

    ! Write the global attributes
    err_stat = WriteGAtts( Filename , &
                           FileId   , &
                           Version             = Version             , &
                           Classification_Name = Classification_Name , &
                           Title               = Title               , &
                           History             = History             , &
                           Comment             = Comment               )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing global attribute to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF

    ! Define the variables
    ! ...Surface Type variable
    NF90_Status = NF90_DEF_VAR( FileID, &
      TYPE_VARNAME, &
      TYPE_TYPE, &
      dimIDs=(/tnsl_DimID, n_Surface_Types_DimID/), &
      varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//TYPE_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,TYPE_LONGNAME    )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,TYPE_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,TYPE_UNITS       )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,TYPE_FILLVALUE   )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//TYPE_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    !...Surface Type IsValid variable
    NF90_Status = NF90_DEF_VAR( FileID, &
      TYPE_ISVALID_VARNAME, &
      TYPE_ISVALID_TYPE, &
      dimIDs=(/n_Surface_Types_DimID/), &
      varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//TYPE_ISVALID_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,TYPE_ISVALID_LONGNAME    )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,TYPE_ISVALID_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,TYPE_ISVALID_UNITS       )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,TYPE_ISVALID_FILLVALUE   )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//TYPE_ISVALID_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Frequency variable
    NF90_Status = NF90_DEF_VAR( FileID, &
      FREQUENCY_VARNAME, &
      FREQUENCY_TYPE, &
      dimIDs=(/n_Frequencies_DimID/), &
      varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//FREQUENCY_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,FREQUENCY_LONGNAME    )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,FREQUENCY_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,FREQUENCY_UNITS       )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,FREQUENCY_FILLVALUE   )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//FREQUENCY_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
    ! ...Reflectance variable
    NF90_Status = NF90_DEF_VAR( FileID, &
      REFLECTANCE_VARNAME, &
      REFLECTANCE_TYPE, &
      dimIDs=(/n_Frequencies_DimID, n_Surface_Types_DimID/), &
      varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//REFLECTANCE_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,REFLECTANCE_LONGNAME    )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,REFLECTANCE_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,REFLECTANCE_UNITS       )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,REFLECTANCE_FILLVALUE   )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//REFLECTANCE_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF

    ! Take netCDF file out of define mode
    NF90_Status = NF90_ENDDEF( FileId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error taking file '//TRIM(Filename)// &
            ' out of define mode - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF

  CONTAINS

    SUBROUTINE Create_CleanUp()
      IF ( Close_File ) THEN
        NF90_Status = NF90_CLOSE( FileID )
        IF ( NF90_Status /= NF90_NOERR ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup - '//&
                TRIM(NF90_STRERROR( NF90_Status ))
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Create_CleanUp

  END FUNCTION CreateFile

END MODULE SEcategory_netCDF_IO
