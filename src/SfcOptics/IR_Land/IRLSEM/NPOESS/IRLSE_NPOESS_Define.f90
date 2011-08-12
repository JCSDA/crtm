!
! IRLSE_NPOESS_Define
!
! Module defining the IRLSE_NPOESS object.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 11-Aug-2009
!                       paul.vandelst@noaa.gov
 
MODULE IRLSE_NPOESS_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds           , ONLY: fp, Long, Double
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE Compare_Float_Numbers, ONLY: OPERATOR(.EqualTo.)
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Datatypes
  PUBLIC :: IRLSE_NPOESS_type
  ! Operators
  PUBLIC :: OPERATOR(==)
  ! Procedures
  PUBLIC :: IRLSE_NPOESS_Associated
  PUBLIC :: IRLSE_NPOESS_Destroy
  PUBLIC :: IRLSE_NPOESS_Create
  PUBLIC :: IRLSE_NPOESS_Inspect
  PUBLIC :: IRLSE_NPOESS_ValidRelease
  PUBLIC :: IRLSE_NPOESS_Info
  PUBLIC :: IRLSE_NPOESS_DefineVersion
  PUBLIC :: IRLSE_NPOESS_SetValue
  PUBLIC :: IRLSE_NPOESS_GetValue


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE IRLSE_NPOESS_Equal
  END INTERFACE OPERATOR(==)


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
    '$Id$'
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp
  ! String lengths
  INTEGER,  PARAMETER :: ML = 256 ! Message length
  INTEGER,  PARAMETER :: SL =  20 ! Surface type name string length
  ! Current valid release and version
  INTEGER, PARAMETER :: IRLSE_NPOESS_RELEASE = 1  ! This determines structure and file formats.
  INTEGER, PARAMETER :: IRLSE_NPOESS_VERSION = 1  ! This is just the default data version.


  ! ----------------------------------
  ! IRLSE_NPOESS data type definitions
  ! ----------------------------------
  TYPE :: IRLSE_NPOESS_type
    PRIVATE
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! Release and version information
    INTEGER(Long) :: Release = IRLSE_NPOESS_RELEASE
    INTEGER(Long) :: Version = IRLSE_NPOESS_VERSION
    ! Dimensions
    INTEGER(Long) :: String_Length   = SL
    INTEGER(Long) :: n_Frequencies   = 0  ! L dim.
    INTEGER(Long) :: n_Surface_Types = 0  ! N dim.
    ! Dimensional vectors
    REAL(Double),  ALLOCATABLE :: Frequency(:)         ! Lx1
    CHARACTER(SL), ALLOCATABLE :: Surface_Type_Name(:) ! Nx1
    ! Reflectance LUT data
    REAL(Double),  ALLOCATABLE :: Reflectance(:,:)     ! LxN
  END TYPE IRLSE_NPOESS_type


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
!       IRLSE_NPOESS_Associated
!
! PURPOSE:
!       Elemental function to test the status of the allocatable components
!       of the IRLSE_NPOESS structure.
!
! CALLING SEQUENCE:
!       Status = IRLSE_NPOESS_Associated( IRLSE_NPOESS )
!
! OBJECTS:
!       IRLSE_NPOESS:  Structure which is to have its member's
!                      status tested.
!                      UNITS:      N/A
!                      TYPE:       IRLSE_NPOESS_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:     The return value is a logical value indicating the
!                   status of the NLTE members.
!                    .TRUE.  - if ANY of the IRLSE_NPOESS allocatable members
!                              are in use.
!                    .FALSE. - if ALL of the IRLSE_NPOESS allocatable members
!                              are not in use.
!                   UNITS:      N/A
!                   TYPE:       LOGICAL
!                   DIMENSION:  Same as input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION IRLSE_NPOESS_Associated( self ) RESULT( Status )
    TYPE(IRLSE_NPOESS_type), INTENT(IN) :: self
    LOGICAL :: Status
    Status = self%Is_Allocated
  END FUNCTION IRLSE_NPOESS_Associated

  
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IRLSE_NPOESS_Destroy
!
! PURPOSE:
!       Elemental subroutine to re-initialize IRLSE_NPOESS objects.
!
! CALLING SEQUENCE:
!       CALL IRLSE_NPOESS_Destroy( IRLSE_NPOESS )
!
! OBJECTS:
!       IRLSE_NPOESS: Re-initialized IRLSE_NPOESS structure.
!                     UNITS:      N/A
!                     TYPE:       IRLSE_NPOESS_type
!                     DIMENSION:  Scalar or any rank
!                     ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE IRLSE_NPOESS_Destroy( self )
    TYPE(IRLSE_NPOESS_type), INTENT(OUT) :: self
    self%Is_Allocated = .FALSE.
    self%n_Frequencies   = 0
    self%n_Surface_Types = 0
  END SUBROUTINE IRLSE_NPOESS_Destroy


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IRLSE_NPOESS_Create
!
! PURPOSE:
!       Elemental subroutine to create an instance of an IRLSE_NPOESS object.
!
! CALLING SEQUENCE:
!       CALL IRLSE_NPOESS_Create( IRLSE_NPOESS   , &
!                                 n_Frequencies  , &     
!                                 n_Surface_Types  )         
!
! OBJECTS:
!       IRLSE_NPOESS:       IRLSE_NPOESS object structure.
!                           UNITS:      N/A
!                           TYPE:       IRLSE_NPOESS_type
!                           DIMENSION:  Scalar or any rank
!                           ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_Frequencies:      Number of spectral frequencies for which there are
!                           reflectance data.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Conformable with the IRLSE_NPOESS object
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Surface_Types:    Number of land surface types for which is are
!                           reflectance data.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Conformable with the IRLSE_NPOESS object
!                           ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE IRLSE_NPOESS_Create( &
    self           , &  ! Output
    n_Frequencies  , &  ! Input
    n_Surface_Types  )  ! Input
    ! Arguments
    TYPE(IRLSE_NPOESS_type), INTENT(OUT) :: self
    INTEGER                , INTENT(IN)  :: n_Frequencies          
    INTEGER                , INTENT(IN)  :: n_Surface_Types             
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_Frequencies   < 1 .OR. &
         n_Surface_Types < 1 ) RETURN

   
    ! Perform the allocation
    ALLOCATE( self%Frequency( n_Frequencies ), &
              self%Surface_Type_Name( n_Surface_Types ), &
              self%Reflectance( n_Frequencies, n_Surface_Types ), &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN


    ! Initialise
    ! ...Dimensions
    self%n_Frequencies   = n_Frequencies  
    self%n_Surface_Types = n_Surface_Types
    ! ...Arrays
    self%Frequency         = ZERO
    self%Surface_Type_Name = ''
    self%Reflectance       = ZERO

    ! Set allocation indicator
    self%Is_Allocated = .TRUE.

  END SUBROUTINE IRLSE_NPOESS_Create


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IRLSE_NPOESS_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of a IRLSE_NPOESS object to stdout.
!
! CALLING SEQUENCE:
!       CALL IRLSE_NPOESS_Inspect( IRLSE_NPOESS )
!
! OBJECTS:
!       IRLSE_NPOESS:  IRLSE_NPOESS object to display.
!                      UNITS:      N/A
!                      TYPE:       IRLSE_NPOESS_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE IRLSE_NPOESS_Inspect( self)
    TYPE(IRLSE_NPOESS_type), INTENT(IN) :: self
    INTEGER :: n
    WRITE(*,'(1x,"IRLSE_NPOESS OBJECT")')
    ! Release/version info
    WRITE(*,'(3x,"Release.Version  :",1x,i0,".",i0)') self%Release, self%Version
    ! Dimensions
    WRITE(*,'(3x,"n_Frequencies    :",1x,i0)') self%n_Frequencies
    WRITE(*,'(3x,"n_Surface_Types  :",1x,i0)') self%n_Surface_Types
    IF ( .NOT. IRLSE_NPOESS_Associated(self) ) RETURN
    ! Dimension arrays
    WRITE(*,'(3x,"Frequency :")')
    WRITE(*,'(5(1x,es13.6,:))') self%Frequency
    WRITE(*,'(3x,"Surface_Type_Name :")')
    WRITE(*,'(4(a,:))') self%Surface_Type_Name
    ! Reflectance array
    WRITE(*,'(3x,"Reflectance :")')
    DO n = 1, self%n_Surface_Types
      WRITE(*,'(5x,a)') self%Surface_Type_Name(n)
      WRITE(*,'(5(1x,es13.6,:))') self%Reflectance(:,n)
    END DO
  END SUBROUTINE IRLSE_NPOESS_Inspect



!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IRLSE_NPOESS_ValidRelease
!
! PURPOSE:
!       Function to check the IRLSE_NPOESS Release value.
!
! CALLING SEQUENCE:
!       IsValid = IRLSE_NPOESS_ValidRelease( IRLSE_NPOESS )
!
! INPUTS:
!       IRLSE_NPOESS:  IRLSE_NPOESS object for which the Release component
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       IRLSE_NPOESS_type
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

  FUNCTION IRLSE_NPOESS_ValidRelease( self ) RESULT( IsValid )
    ! Arguments
    TYPE(IRLSE_NPOESS_type), INTENT(IN) :: self
    ! Function result
    LOGICAL :: IsValid
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'IRLSE_NPOESS_ValidRelease'
    ! Local variables
    CHARACTER(ML) :: msg

    ! Set up
    IsValid = .TRUE.


    ! Check release is not too old
    IF ( self%Release < IRLSE_NPOESS_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("An IRLSE_NPOESS data update is needed. ", &
                  &"IRLSE_NPOESS release is ",i0,". Valid release is ",i0,"." )' ) &
                  self%Release, IRLSE_NPOESS_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION ); RETURN
    END IF


    ! Check release is not too new
    IF ( self%Release > IRLSE_NPOESS_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("An IRLSE_NPOESS software update is needed. ", &
                  &"IRLSE_NPOESS release is ",i0,". Valid release is ",i0,"." )' ) &
                  self%Release, IRLSE_NPOESS_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION ); RETURN
    END IF

  END FUNCTION IRLSE_NPOESS_ValidRelease


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IRLSE_NPOESS_Info
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about a IRLSE_NPOESS object.
!
! CALLING SEQUENCE:
!       CALL IRLSE_NPOESS_Info( IRLSE_NPOESS, Info )
!
! OBJECTS:
!       IRLSE_NPOESS:  IRLSE_NPOESS object about which info is required.
!                      UNITS:      N/A
!                      TYPE:       IRLSE_NPOESS_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Info:          String containing version and dimension information
!                      about the IRLSE_NPOESS object.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE IRLSE_NPOESS_Info( self, Info )
    ! Arguments
    TYPE(IRLSE_NPOESS_type), INTENT(IN)  :: self
    CHARACTER(*),            INTENT(OUT) :: Info
    ! Parameters
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    ! Local variables
    CHARACTER(2000) :: Long_String

    ! Write the required data to the local string
    WRITE( Long_String, &
           '(a,1x,"IRLSE_NPOESS RELEASE.VERSION: ",i2,".",i2.2,a,3x, &
           &"N_FREQUENCIES=",i0,2x,&
           &"N_SURFACE_TYPES=",i0 )' ) &
           ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
           self%Release, self%Version, &
           ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
           self%n_Frequencies , &
           self%n_Surface_Types
                       
    ! Trim the output based on the
    ! dummy argument string length
    Info = Long_String(1:MIN(LEN(Info), LEN_TRIM(Long_String)))

  END SUBROUTINE IRLSE_NPOESS_Info
 
 
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IRLSE_NPOESS_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL IRLSE_NPOESS_DefineVersion( Id )
!
! OUTPUTS:
!       Id:    Character string containing the version Id information
!              for the module.
!              UNITS:      N/A
!              TYPE:       CHARACTER(*)
!              DIMENSION:  Scalar
!              ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE IRLSE_NPOESS_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE IRLSE_NPOESS_DefineVersion



!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IRLSE_NPOESS_SetValue
!
! PURPOSE:
!       Subroutine to set the contents of a valid IRLSE_NPOESS object.
!
! CALLING SEQUENCE:
!       CALL IRLSE_NPOESS_SetValue( IRLSE_NPOESS, &
!                                   Version           = Version          , &
!                                   Frequency         = Frequency        , &
!                                   Surface_Type_Name = Surface_Type_Name, &
!                                   Reflectance       = Reflectance        )
!
! OBJECTS:
!       IRLSE_NPOESS:       Valid, allocated IRLSE_NPOESS object for which
!                           values are to be set.
!                           UNITS:      N/A
!                           TYPE:       IRLSE_NPOESS_type
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUTS:
!       Version:            Integer indicating the data version. If not specified
!                           the value of the module parameter IRLSE_NPOESS_VERSION
!                           is used.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Frequency:          Real array to which the Frequency component of the
!                           IRLSE_NPOESS object is to be set. The size of the
!                           input must match the allocated size of the component,
!                           otherwise all the component values are set to zero.
!                           UNITS:      N/A
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Rank-1 (L)
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Surface_Type_Name:  Character array to which the Surface_Type_Name component
!                           of the IRLSE_NPOESS object is to be set. The size of the
!                           input must match the allocated size of the component,
!                           otherwise all the component values are set to a blank string.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Rank-1 (N)
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Reflectance:        Real array to which the Reflectance component of the
!                           IRLSE_NPOESS object is to be set. The size of the
!                           input must match the allocated size of the component,
!                           otherwise all the component values are set to zero.
!                           UNITS:      N/A
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Rank-2 (L x N)
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE IRLSE_NPOESS_SetValue( &
    self             , &  ! Input
    Version          , &  ! Optional input
    Frequency        , &  ! Optional input
    Surface_Type_Name, &  ! Optional input
    Reflectance        )  ! Optional input
    ! Arguments
    TYPE(IRLSE_NPOESS_type), INTENT(IN OUT) :: self
    INTEGER     ,  OPTIONAL, INTENT(IN)     :: Version
    REAL(fp)    ,  OPTIONAL, INTENT(IN)     :: Frequency(:)
    CHARACTER(*),  OPTIONAL, INTENT(IN)     :: Surface_Type_Name(:)
    REAL(fp)    ,  OPTIONAL, INTENT(IN)     :: Reflectance(:,:)
   
    IF ( .NOT. IRLSE_NPOESS_Associated(self) ) RETURN

    IF ( PRESENT(Version) ) self%Version = Version
    
    IF ( PRESENT(Frequency) ) THEN
      IF ( SIZE(Frequency) == self%n_Frequencies ) THEN
        self%Frequency = Frequency
      ELSE
        self%Frequency = ZERO
      END IF
    END IF
   
    IF ( PRESENT(Surface_Type_Name) ) THEN
      IF ( SIZE(Surface_Type_Name) == self%n_Surface_Types ) THEN
        self%Surface_Type_Name = Surface_Type_Name
      ELSE
        self%Surface_Type_Name = ''
      END IF
    END IF
   
    IF ( PRESENT(Reflectance) ) THEN
      IF ( SIZE(Reflectance,DIM=1) == self%n_Frequencies .AND. &
           SIZE(Reflectance,DIM=2) == self%n_Surface_Types ) THEN
        self%Reflectance = Reflectance
      ELSE
        self%Reflectance = ZERO
      END IF
    END IF
   
  END SUBROUTINE IRLSE_NPOESS_SetValue
 

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IRLSE_NPOESS_GetValue
!
! PURPOSE:
!       Subroutine to get the contents of a valid IRLSE_NPOESS object.
!
! CALLING SEQUENCE:
!       CALL IRLSE_NPOESS_GetValue( IRLSE_NPOESS, &
!                                   Surface_Type_Name_ToGet  = Surface_Type_Name_ToGet, &
!                                   Version                  = Version                , &
!                                   n_Frequencies            = n_Frequencies          , &
!                                   n_Surface_Types          = n_Surface_Types        , &
!                                   Frequency                = Frequency              , &
!                                   Surface_Type_Name        = Surface_Type_Name      , &
!                                   Reflectance              = Reflectance            , &
!                                   Surface_Reflectance      = Surface_Reflectance      )
!
! OBJECTS:
!       IRLSE_NPOESS:            Valid, allocated IRLSE_NPOESS object from which
!                                values are to be retrieved.
!                                UNITS:      N/A
!                                TYPE:       IRLSE_NPOESS_type
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUTS:
!       Surface_Type_Name_ToGet: Character string containing a valid surface type
!                                name in the IRLSE_NPOESS object.
!                                NOTE: - This argument is used in conjuction with
!                                        the Surface_Reflectance dummy output
!                                        argument to retrieve the reflectance of a
!                                        particular surface type.
!                                      - This argument is ignored if the optional
!                                        Surface_Reflectance argument is not also
!                                        provided.
!                                UNITS:      N/A
!                                TYPE:       CHARACTER(*)
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! OPTIONAL OUTPUTS:
!       Version:                 Integer indicating the data version of the object.
!                                UNITS:      N/A
!                                TYPE:       INTEGER
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Frequency:               Real array to which the Frequency component of the
!                                IRLSE_NPOESS object will be assigned. The actual
!                                argument must be declared as allocatable.
!                                UNITS:      N/A
!                                TYPE:       REAL(fp)
!                                DIMENSION:  Rank-1 (L)
!                                ATTRIBUTES: INTENT(OUT), OPTIONAL, ALLOCATABLE
!
!       Surface_Type_Name:       Character array to which the Surface_Type_Name component
!                                of the IRLSE_NPOESS object will be assigned. The actual
!                                argument must be declared as allocatable.
!                                UNITS:      N/A
!                                TYPE:       CHARACTER(*)
!                                DIMENSION:  Rank-1 (N)
!                                ATTRIBUTES: INTENT(OUT), OPTIONAL, ALLOCATABLE
!
!       Reflectance:             Real array to which the Reflectance component of the
!                                IRLSE_NPOESS object will be assigned. The actual
!                                argument must be declared as allocatable.
!                                UNITS:      N/A
!                                TYPE:       REAL(fp)
!                                DIMENSION:  Rank-2 (L x N)
!                                ATTRIBUTES: INTENT(OUT), OPTIONAL, ALLOCATABLE
!
!       Surface_Reflectance:     Real array to which the Reflectance component for a
!                                given surface type in IRLSE_NPOESS object will be
!                                assigned. The actual argument must be declared as
!                                allocatable.
!                                NOTE: - This argument is used in conjuction with
!                                        the Surface_Type_Name_ToGet dummy input
!                                        argument to retrieve the reflectance of a
!                                        particular surface type.
!                                      - This argument is ignored if the optional
!                                        Surface_Type_Name_ToGet argument is not
!                                        also provided.
!                                UNITS:      N/A
!                                TYPE:       REAL(fp)
!                                DIMENSION:  Rank-1 (L)
!                                ATTRIBUTES: INTENT(OUT), OPTIONAL, ALLOCATABLE
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE IRLSE_NPOESS_GetValue( &
    self                   , &  ! Input
    Surface_Type_Name_ToGet, &  ! Optional input
    Version                , &  ! Optional output
    n_Frequencies          , &  ! Optional output
    n_Surface_Types        , &  ! Optional output
    Frequency              , &  ! Optional output
    Surface_Type_Name      , &  ! Optional output
    Reflectance            , &  ! Optional output
    Surface_Reflectance      )  ! Optional output
    ! Arguments
    TYPE(IRLSE_NPOESS_type),             INTENT(IN OUT) :: self
    CHARACTER(*),              OPTIONAL, INTENT(IN)     :: Surface_Type_Name_ToGet
    INTEGER     ,              OPTIONAL, INTENT(OUT)    :: Version
    INTEGER     ,              OPTIONAL, INTENT(OUT)    :: n_Frequencies
    INTEGER     ,              OPTIONAL, INTENT(OUT)    :: n_Surface_Types
    REAL(fp)    , ALLOCATABLE, OPTIONAL, INTENT(OUT)    :: Frequency(:)
    CHARACTER(*), ALLOCATABLE, OPTIONAL, INTENT(OUT)    :: Surface_Type_Name(:)
    REAL(fp)    , ALLOCATABLE, OPTIONAL, INTENT(OUT)    :: Reflectance(:,:)
    REAL(fp)    , ALLOCATABLE, OPTIONAL, INTENT(OUT)    :: Surface_Reflectance(:)
    
    IF ( .NOT. IRLSE_NPOESS_Associated(self) ) RETURN
   
    IF ( PRESENT(Version        ) ) Version         = self%Version
    IF ( PRESENT(n_Frequencies  ) ) n_Frequencies   = self%n_Frequencies
    IF ( PRESENT(n_Surface_Types) ) n_Surface_Types = self%n_Surface_Types

    IF ( PRESENT(Frequency) ) THEN
      ALLOCATE(Frequency(self%n_Frequencies))
      Frequency = self%Frequency
    END IF

    IF ( PRESENT(Surface_Type_Name) ) THEN
      ALLOCATE(Surface_Type_Name(self%n_Surface_Types))
      Surface_Type_Name = self%Surface_Type_Name
    END IF

    IF ( PRESENT(Reflectance) ) THEN
      ALLOCATE(Reflectance(self%n_Frequencies, self%n_Surface_Types))
      Reflectance = self%Reflectance
    END IF
    
    IF ( PRESENT(Surface_Type_Name_ToGet) .AND. PRESENT(Surface_Reflectance) ) THEN
      ! Match surface type and assign
    END IF

  END SUBROUTINE IRLSE_NPOESS_GetValue
 

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
!       IRLSE_NPOESS_Equal
!
! PURPOSE:
!       Elemental function to test the equality of two IRLSE_NPOESS objects.
!       Used in OPERATOR(==) interface block.
!
! CALLING SEQUENCE:
!       is_equal = IRLSE_NPOESS_Equal( x, y )
!
!         or
!
!       IF ( x == y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:          Two IRLSE_NPOESS objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       IRLSE_NPOESS_type
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

  ELEMENTAL FUNCTION IRLSE_NPOESS_Equal( x, y ) RESULT( is_equal )
    TYPE(IRLSE_NPOESS_type), INTENT(IN) :: x, y
    LOGICAL :: is_equal

    ! Set up
    is_equal = .FALSE.
   
    ! Check the object association status
    IF ( (.NOT. IRLSE_NPOESS_Associated(x)) .OR. &
         (.NOT. IRLSE_NPOESS_Associated(y))      ) RETURN

   ! Check contents
    ! ...Release/version info
    IF ( (x%Release /= y%Release) .OR. &
         (x%Version /= y%Version) ) RETURN
    ! ...Dimensions
    IF ( (x%n_Frequencies   /= y%n_Frequencies   ) .OR. &
         (x%n_Surface_Types /= y%n_Surface_Types ) ) RETURN
    ! ...Arrays
    IF ( ALL(x%Frequency         .EqualTo. y%Frequency         ) .AND. &
         ALL(x%Surface_Type_Name     ==    y%Surface_Type_Name ) .AND. &
         ALL(x%Reflectance       .EqualTo. y%Reflectance       ) ) &
      is_equal = .TRUE.

  END FUNCTION IRLSE_NPOESS_Equal

END MODULE IRLSE_NPOESS_Define
