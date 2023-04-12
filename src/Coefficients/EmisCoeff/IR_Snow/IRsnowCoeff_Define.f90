!
! IRsnowCoeff_Define
!
! Module defining the IRsnowCoeff object to hold coefficient
! data for the infrared snow surface emissivity and reflectivity models.
!
!
! CREATION HISTORY:
!       Written by:     Cheng Dang, 18-May-2022
!                       dangch@ucar.edu

MODULE IRsnowCoeff_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds           , ONLY: fp, Long, Double
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE Compare_Float_Numbers, ONLY: OPERATOR(.EqualTo.)
  USE File_Utility         , ONLY: File_Open, File_Exists
  USE Binary_File_Utility  , ONLY: Open_Binary_File      , &
                                   WriteGAtts_Binary_File, &
                                   ReadGAtts_Binary_File
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Datatypes
  PUBLIC :: IRsnowCoeff_type
  ! Operators
  PUBLIC :: OPERATOR(==)
  ! Procedures
  PUBLIC :: IRsnowCoeff_Associated
  PUBLIC :: IRsnowCoeff_Destroy
  PUBLIC :: IRsnowCoeff_Create
  PUBLIC :: IRsnowCoeff_Inspect
  PUBLIC :: IRsnowCoeff_ValidRelease
  PUBLIC :: IRsnowCoeff_Info


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE IRsnowCoeff_Equal
  END INTERFACE OPERATOR(==)


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Current valid release and version
  INTEGER, PARAMETER :: IRsnowCOEFF_RELEASE = 1  ! This determines structure and file formats.
  INTEGER, PARAMETER :: IRsnowCOEFF_VERSION = 1  ! This is just the default data version.
  ! Close status for write errors
  CHARACTER(*), PARAMETER :: WRITE_ERROR_STATUS = 'DELETE'
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp
  ! String lengths
  INTEGER,  PARAMETER :: ML = 256 ! Message length


  ! ----------------------------------
  ! IRsnowCoeff data type definitions
  ! ----------------------------------
  !:tdoc+:
  TYPE :: IRsnowCoeff_type
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! Release and version information
    INTEGER(Long) :: Release = IRsnowCOEFF_RELEASE
    INTEGER(Long) :: Version = IRsnowCOEFF_VERSION
    ! Surface classification name
    CHARACTER(ML) :: Classification_Name = ''
    ! Dimensions
    INTEGER(Long) :: n_Angles      = 0   ! I dimension
    INTEGER(Long) :: n_Frequencies = 0   ! L dimension
    INTEGER(Long) :: n_Grain_Sizes = 0   ! G dimension
    INTEGER(Long) :: n_Temperature = 0   ! T dimension
    ! Dimensional vectors
    REAL(Double), ALLOCATABLE :: Angle(:)        ! I
    REAL(Double), ALLOCATABLE :: Frequency(:)    ! L
    REAL(Double), ALLOCATABLE :: Grain_Size(:)   ! G
    REAL(Double), ALLOCATABLE :: Temperature(:)  ! T
    ! Emissivity LUT data
    REAL(Double), ALLOCATABLE :: Emissivity(:,:,:,:)  ! I x L x G x T
  END TYPE IRsnowCoeff_type
  !:tdoc-:


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
!       IRsnowCoeff_Associated
!
! PURPOSE:
!       Elemental function to test the status of the allocatable components
!       of the IRsnowCoeff structure.
!
! CALLING SEQUENCE:
!       Status = IRsnowCoeff_Associated( IRsnowCoeff )
!
! OBJECTS:
!       IRsnowCoeff:  Structure which is to have its member's
!                      status tested.
!                      UNITS:      N/A
!                      TYPE:       IRsnowCoeff_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:        The return value is a logical value indicating the
!                      status of the NLTE members.
!                       .TRUE.  - if ANY of the IRsnowCoeff allocatable members
!                                 are in use.
!                       .FALSE. - if ALL of the IRsnowCoeff allocatable members
!                                 are not in use.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Same as input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION IRsnowCoeff_Associated( self ) RESULT( Status )
    TYPE(IRsnowCoeff_type), INTENT(IN) :: self
    LOGICAL :: Status
    Status = self%Is_Allocated
  END FUNCTION IRsnowCoeff_Associated


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IRsnowCoeff_Destroy
!
! PURPOSE:
!       Elemental subroutine to re-initialize IRsnowCoeff objects.
!
! CALLING SEQUENCE:
!       CALL IRsnowCoeff_Destroy( IRsnowCoeff )
!
! OBJECTS:
!       IRsnowCoeff: Re-initialized IRsnowCoeff structure.
!                     UNITS:      N/A
!                     TYPE:       IRsnowCoeff_type
!                     DIMENSION:  Scalar or any rank
!                     ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE IRsnowCoeff_Destroy( self )
    TYPE(IRsnowCoeff_type), INTENT(OUT) :: self
    self%Is_Allocated = .FALSE.
  END SUBROUTINE IRsnowCoeff_Destroy


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IRsnowCoeff_Create
!
! PURPOSE:
!       Elemental subroutine to create an instance of an IRsnowCoeff object.
!
! CALLING SEQUENCE:
!       CALL IRsnowCoeff_Create( IRsnowCoeff   , &
!                                 n_Angles     , &
!                                 n_Frequencies, &
!                                 n_Grain_Sizes, &
!                                 n_Temperature  )
!
! OBJECTS:
!       IRsnowCoeff:   IRsnowCoeff object structure.
!                       UNITS:      N/A
!                       TYPE:       IRsnowCoeff_type
!                       DIMENSION:  Scalar or any rank
!                       ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_Angles:       Number of angles dimension.
!                       Must be > 0.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Conformable with the IRsnowCoeff object
!                       ATTRIBUTES: INTENT(IN)
!
!       n_Frequencies:  Number of frequencies dimension.
!                       Must be > 0.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Conformable with the IRsnowCoeff object
!                       ATTRIBUTES: INTENT(IN)
!
!       n_Grain_Sizes:  Number of Grain Sizes dimension.
!                       Must be > 0.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Conformable with the IRsnowCoeff object
!                       ATTRIBUTES: INTENT(IN)
!
!       n_Temperature:  Number oftemperature dimension.
!                       Must be > 0.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Conformable with the IRsnowCoeff object
!                       ATTRIBUTES: INTENT(IN)
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE IRsnowCoeff_Create( &
    self         , &  ! Output
    n_Angles     , &  ! Input
    n_Frequencies, &  ! Input
    n_Grain_Sizes, &  ! Input
    n_Temperature  )  ! Input
    ! Arguments
    TYPE(IRsnowCoeff_type) , INTENT(OUT) :: self
    INTEGER                , INTENT(IN)  :: n_Angles
    INTEGER                , INTENT(IN)  :: n_Frequencies
    INTEGER                , INTENT(IN)  :: n_Grain_Sizes
    INTEGER                , INTENT(IN)  :: n_Temperature
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( self%Is_Allocated .OR. &
         n_Angles      < 1 .OR. &
         n_Frequencies < 1 .OR. &
         n_Grain_Sizes < 1 .OR. &
         n_Temperature < 1) RETURN

    ! Perform the allocation
    ALLOCATE( self%Angle( n_Angles ), &
              self%Frequency( n_Frequencies ), &
              self%Grain_Size( n_Grain_Sizes ), &
              self%Temperature( n_Temperature ), &
              self%Emissivity( n_Angles, n_Frequencies, n_Grain_Sizes, n_Temperature), &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN


    ! Initialise
    ! ...Dimensions
    self%n_Angles      = n_Angles
    self%n_Frequencies = n_Frequencies
    self%n_Grain_Sizes = n_Grain_Sizes
    self%n_Temperature = n_Temperature
    ! ...Arrays
    self%Angle        = ZERO
    self%Frequency    = ZERO
    self%Grain_Size   = ZERO
    self%Temperature  = ZERO
    self%Emissivity   = ZERO

    ! Set allocation indicator
    self%Is_Allocated = .TRUE.

  END SUBROUTINE IRsnowCoeff_Create


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IRsnowCoeff_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of a IRsnowCoeff object to stdout.
!
! CALLING SEQUENCE:
!       CALL IRsnowCoeff_Inspect( IRsnowCoeff )
!
! OBJECTS:
!       IRsnowCoeff:  IRsnowCoeff object to display.
!                      UNITS:      N/A
!                      TYPE:       IRsnowCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE IRsnowCoeff_Inspect( self )
    TYPE(IRsnowCoeff_type), INTENT(IN) :: self
    INTEGER :: i2, i3, i4
    WRITE(*,'(1x,"IRsnowCoeff OBJECT")')
    ! Release/version info
    WRITE(*,'(3x,"Release.Version :",1x,i0,".",i0)') self%Release, self%Version
    ! Surface classification name
    WRITE(*,'(3x,"Classification_Name :",1x,a)') TRIM(self%Classification_Name)
    ! Dimensions
    WRITE(*,'(3x,"n_Angles        :",1x,i0)') self%n_Angles
    WRITE(*,'(3x,"n_Frequencies   :",1x,i0)') self%n_Frequencies
    WRITE(*,'(3x,"n_Grain_Sizes   :",1x,i0)') self%n_Grain_Sizes
    WRITE(*,'(3x,"n_Temperature   :",1x,i0)') self%n_Temperature
    IF ( .NOT. IRsnowCoeff_Associated(self) ) RETURN
    ! Dimension arrays
    WRITE(*,'(3x,"Angle      :")')
    WRITE(*,'(5(1x,es22.15,:))') self%Angle
    WRITE(*,'(3x,"Frequency  :")')
    WRITE(*,'(5(1x,es22.15,:))') self%Frequency
    WRITE(*,'(3x,"Grain_Size :")')
    WRITE(*,'(5(1x,es22.15,:))') self%Grain_Size
    WRITE(*,'(3x,"Temperature :")')
    WRITE(*,'(5(1x,es22.15,:))') self%Temperature
    ! Emissivity array
    WRITE(*,'(3x,"Emissivity :")')
    DO i4 = 1, self%n_Temperature
      WRITE(*,'(5x,"TEMPERATURE :",es22.15)') self%Temperature(i4)
      DO i3 = 1, self%n_Grain_Sizes
        WRITE(*,'(5x,"Grain_Size :",es22.15)') self%Grain_Size(i3)
        DO i2 = 1, self%n_Frequencies
          WRITE(*,'(5x,"FREQUENCY  :",es22.15)') self%Frequency(i2)
          WRITE(*,'(5(1x,es22.15,:))') self%Emissivity(:,i2,i3,i4)
        END DO
      END DO
    END DO
  END SUBROUTINE IRsnowCoeff_Inspect



!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IRsnowCoeff_ValidRelease
!
! PURPOSE:
!       Function to check the IRsnowCoeff Release value.
!
! CALLING SEQUENCE:
!       IsValid = IRsnowCoeff_ValidRelease( IRsnowCoeff )
!
! INPUTS:
!       IRsnowCoeff:  IRsnowCoeff object for which the Release component
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       IRsnowCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       IsValid:       Logical value defining the release validity.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Scalar
!
!:sdoc-:
!----------------------------------------------------------------------------------

  FUNCTION IRsnowCoeff_ValidRelease( self ) RESULT( IsValid )
    ! Arguments
    TYPE(IRsnowCoeff_type), INTENT(IN) :: self
    ! Function result
    LOGICAL :: IsValid
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'IRsnowCoeff_ValidRelease'
    ! Local variables
    CHARACTER(ML) :: msg

    ! Set up
    IsValid = .TRUE.


    ! Check release is not too old
    IF ( self%Release < IRsnowCOEFF_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("An IRsnowCoeff data update is needed. ", &
                  &"IRsnowCoeff release is ",i0,". Valid release is ",i0,"." )' ) &
                  self%Release, IRsnowCOEFF_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION ); RETURN
    END IF


    ! Check release is not too new
    IF ( self%Release > IRsnowCOEFF_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("An IRsnowCoeff software update is needed. ", &
                  &"IRsnowCoeff release is ",i0,". Valid release is ",i0,"." )' ) &
                  self%Release, IRsnowCOEFF_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION ); RETURN
    END IF

  END FUNCTION IRsnowCoeff_ValidRelease


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IRsnowCoeff_Info
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about a IRsnowCoeff object.
!
! CALLING SEQUENCE:
!       CALL IRsnowCoeff_Info( IRsnowCoeff, Info )
!
! OBJECTS:
!       IRsnowCoeff:  IRsnowCoeff object about which info is required.
!                      UNITS:      N/A
!                      TYPE:       IRsnowCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Info:          String containing version and dimension information
!                      about the IRsnowCoeff object.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE IRsnowCoeff_Info( self, Info )
    ! Arguments
    TYPE(IRsnowCoeff_type),  INTENT(IN)  :: self
    CHARACTER(*),            INTENT(OUT) :: Info
    ! Parameters
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    ! Local variables
    CHARACTER(2000) :: Long_String

    ! Write the required data to the local string
    WRITE( Long_String, &
           '( a,1x,"IRsnowCoeff RELEASE.VERSION: ", i2, ".", i2.2,a,3x, &
              &"CLASSIFICATION: ",a,",",2x,&
              &"N_ANGLES=",i3,2x,&
              &"N_FREQUENCIES=",i5,2x,&
              &"n_Grain_Sizes=",i3,2x,&
              &"N_TEMPERATURE=",i3 )' ) &
           ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
           self%Release, self%Version, &
           ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
           TRIM(self%Classification_Name), &
           self%n_Angles, &
           self%n_Frequencies, &
           self%n_Grain_Sizes, &
           self%n_Temperature

    ! Trim the output based on the
    ! dummy argument string length
    Info = Long_String(1:MIN(LEN(Info), LEN_TRIM(Long_String)))

  END SUBROUTINE IRsnowCoeff_Info


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!------------------------------------------------------------------------------
!
! NAME:
!       IRsnowCoeff_Equal
!
! PURPOSE:
!       Elemental function to test the equality of two IRsnowCoeff objects.
!       Used in OPERATOR(==) interface block.
!
! CALLING SEQUENCE:
!       is_equal = IRsnowCoeff_Equal( x, y )
!
!         or
!
!       IF ( x == y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:          Two IRsnowCoeff objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       IRsnowCoeff_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       is_equal:      Logical value indicating whether the inputs are equal.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Same as inputs.
!
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION IRsnowCoeff_Equal( x, y ) RESULT( is_equal )
    TYPE(IRsnowCoeff_type), INTENT(IN) :: x, y
    LOGICAL :: is_equal

    ! Set up
    is_equal = .FALSE.

    ! Check the object association status
    IF ( (.NOT. IRsnowCoeff_Associated(x)) .OR. &
         (.NOT. IRsnowCoeff_Associated(y))      ) RETURN

    ! Check contents
    ! ...Release/version info
    IF ( (x%Release /= y%Release) .OR. &
         (x%Version /= y%Version) ) RETURN
    ! ...Classification name
    IF ( (x%Classification_Name /= y%Classification_Name) ) RETURN
    ! ...Dimensions
    IF ( (x%n_Angles      /= y%n_Angles      ) .OR. &
         (x%n_Frequencies /= y%n_Frequencies ) .OR. &
         (x%n_Grain_Sizes /= y%n_Grain_Sizes ) .OR. &
         (x%n_Temperature /= y%n_Temperature ) ) RETURN
    ! ...Arrays
    IF ( ALL(x%Angle       .EqualTo. y%Angle      ) .AND. &
         ALL(x%Frequency   .EqualTo. y%Frequency  ) .AND. &
         ALL(x%Grain_Size  .EqualTo. y%Grain_Size ) .AND. &
         ALL(x%Temperature .EqualTo. y%Temperature ) .AND. &
         ALL(x%Emissivity  .EqualTo. y%Emissivity ) ) &
      is_equal = .TRUE.

  END FUNCTION IRsnowCoeff_Equal

END MODULE IRsnowCoeff_Define
