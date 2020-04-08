!
! AtmProfile_Define
!
! Module defining the AtmProfile data structure and containing routines
! to manipulate it.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 08-Jul-2002
!                       paul.vandelst@noaa.gov
!

MODULE AtmProfile_Define

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds           , ONLY: Long, Double
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE Compare_Float_Numbers, ONLY: OPERATOR(.EqualTo.)
  USE AtmProfile_Parameters, ONLY: N_ABSORBER_UNITS   , &
                                   ABSORBER_UNITS_ID  , &
                                   ABSORBER_UNITS_NAME, &
                                   ABSORBER_UNITS_CHAR, &
                                   N_ABSORBERS        , &
                                   ABSORBER_NAME

  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Datatypes
  PUBLIC :: AtmProfile_type
  ! Operators
  PUBLIC :: OPERATOR(==)
  PUBLIC :: OPERATOR(/=)
  ! Procedures
  PUBLIC :: AtmProfile_Associated
  PUBLIC :: AtmProfile_Destroy
  PUBLIC :: AtmProfile_Create
  PUBLIC :: AtmProfile_Inspect
  PUBLIC :: AtmProfile_ValidRelease
  PUBLIC :: AtmProfile_Info
  PUBLIC :: AtmProfile_DefineVersion
  PUBLIC :: AtmProfile_Absorber_Name
  PUBLIC :: AtmProfile_Absorber_Units_Name
  PUBLIC :: AtmProfile_Absorber_Units_LBL
  PUBLIC :: AtmProfile_Compare


  ! -------------------
  ! Procedure overloads
  ! -------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE AtmProfile_Equal
  END INTERFACE OPERATOR(==)

  INTERFACE OPERATOR(/=)
    MODULE PROCEDURE AtmProfile_NotEqual
  END INTERFACE OPERATOR(/=)


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Version Id for the module
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! Literal constants
  REAL(Double), PARAMETER :: ZERO = 0.0_Double
  REAL(Double), PARAMETER :: ONE  = 1.0_Double
  ! String lengths
  INTEGER, PARAMETER :: ML = 256  ! Message Length
  INTEGER, PARAMETER :: PL = 512  ! Profile description Length
  INTEGER, PARAMETER :: AL = LEN(ABSORBER_NAME(1))         ! Absorber Name Length
  INTEGER, PARAMETER :: NL = LEN(ABSORBER_UNITS_NAME(0))   ! Absorber unit Name Length
  INTEGER, PARAMETER :: LL = LEN(ABSORBER_UNITS_CHAR(0))   ! Absorber unit LBL Length
  ! Current valid release and version numbers
  INTEGER, PARAMETER :: ATMPROFILE_RELEASE = 3
  INTEGER, PARAMETER :: ATMPROFILE_VERSION = 1


  ! --------------------------
  ! AtmProfile type definition
  ! --------------------------
  !:tdoc+:
  TYPE :: AtmProfile_type
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! Release and version information
    INTEGER(Long) :: Release = ATMPROFILE_RELEASE
    INTEGER(Long) :: Version = ATMPROFILE_VERSION
    ! Dimensions
    INTEGER(Long) :: n_Layers    = 0 ! K
    INTEGER(Long) :: n_Absorbers = 0 ! J
    ! Profile metadata
    INTEGER(Long) :: Profile            = 0
    CHARACTER(PL) :: Description        = ''
    INTEGER(Long) :: Climatology_Model  = 0
    INTEGER(Long) :: Year               = 0
    INTEGER(Long) :: Month              = 0
    INTEGER(Long) :: Day                = 0
    INTEGER(Long) :: Hour               = 0
    REAL(Double)  :: Latitude           = ZERO
    REAL(Double)  :: Longitude          = ZERO
    REAL(Double)  :: Surface_Altitude   = ZERO
    ! Absorber information
    INTEGER(Long), ALLOCATABLE :: Absorber_ID(:)         ! Dimension J
    CHARACTER(AL), ALLOCATABLE :: Absorber_Name(:)       ! Dimension J
    INTEGER(Long), ALLOCATABLE :: Absorber_Units_ID(:)   ! Dimension J
    CHARACTER(NL), ALLOCATABLE :: Absorber_Units_Name(:) ! Dimension J
    CHARACTER(LL), ALLOCATABLE :: Absorber_Units_LBL(:)  ! Dimension J
    ! Profile LEVEL data
    REAL(Double), ALLOCATABLE :: Level_Pressure(:)     ! Dimension 0:K
    REAL(Double), ALLOCATABLE :: Level_Temperature(:)  ! Dimension 0:K
    REAL(Double), ALLOCATABLE :: Level_Absorber(:,:)   ! Dimension 0:K x J
    REAL(Double), ALLOCATABLE :: Level_Altitude(:)     ! Dimension 0:K
    ! Profile LAYER data
    REAL(Double), ALLOCATABLE :: Layer_Pressure(:)     ! Dimension K
    REAL(Double), ALLOCATABLE :: Layer_Temperature(:)  ! Dimension K
    REAL(Double), ALLOCATABLE :: Layer_Absorber(:,:)   ! Dimension K x J
    REAL(Double), ALLOCATABLE :: Layer_Delta_Z(:)      ! Dimension K
  END TYPE AtmProfile_type
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
!       AtmProfile_Associated
!
! PURPOSE:
!       Elemental function to test the status of the allocatable components
!       of an AtmProfile object.
!
! CALLING SEQUENCE:
!       Status = AtmProfile_Associated( AtmProfile )
!
! OBJECTS:
!       AtmProfile:  AtmProfile object which is to have its member's
!                    status tested.
!                    UNITS:      N/A
!                    TYPE:       TYPE(AtmProfile_type)
!                    DIMENSION:  Scalar or any rank
!                    ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:      The return value is a logical value indicating if the
!                    object has been allocated.
!                    UNITS:      N/A
!                    TYPE:       LOGICAL
!                    DIMENSION:  Same as input AtmProfile argument
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION AtmProfile_Associated( self ) RESULT( Status )
    TYPE(AtmProfile_type), INTENT(IN) :: self
    LOGICAL :: Status
    Status = self%Is_Allocated
  END FUNCTION AtmProfile_Associated


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       AtmProfile_Destroy
!
! PURPOSE:
!       Elemental subroutine to re-initialize AtmProfile objects.
!
! CALLING SEQUENCE:
!       CALL AtmProfile_Destroy( AtmProfile )
!
! OBJECTS:
!       AtmProfile:   Re-initialized AtmProfile object.
!                     UNITS:      N/A
!                     TYPE:       TYPE(AtmProfile_type)
!                     DIMENSION:  Scalar or any rank
!                     ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE AtmProfile_Destroy( self )
    TYPE(AtmProfile_type), INTENT(OUT) :: self
    self%Is_Allocated = .FALSE.
  END SUBROUTINE AtmProfile_Destroy


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       AtmProfile_Create
!
! PURPOSE:
!       Elemental subroutine to create an instance of a AtmProfile object.
!
! CALLING SEQUENCE:
!       CALL AtmProfile_Create( AtmProfile , &
!                               n_Layers   , &
!                               n_Absorbers  )
!
! OBJECTS:
!       AtmProfile:   AtmProfile object.
!                     UNITS:      N/A
!                     TYPE:       TYPE(AtmProfile_type)
!                     DIMENSION:  Scalar or any rank
!                     ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_Layers:     Number of atmospheric profile layers.
!                     The "K" dimension. Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Conformable with AtmProfile argument
!                     ATTRIBUTES: INTENT(IN)
!
!       n_Absorbers:  Number of gaseous absorber species.
!                     The "J" dimension. Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Conformable with AtmProfile argument
!                     ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE AtmProfile_Create( &
    self , &
    n_Layers   , &
    n_Absorbers  )
    ! Arguments
    TYPE(AtmProfile_type), INTENT(OUT) :: self
    INTEGER              , INTENT(IN)  :: n_Layers
    INTEGER              , INTENT(IN)  :: n_Absorbers
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_Layers    < 1 .OR. &
         n_Absorbers < 1 ) RETURN


    ! Perform the allocation.
    ALLOCATE( self%Absorber_ID( n_Absorbers )               , &
              self%Absorber_Name( n_Absorbers )             , &
              self%Absorber_Units_ID( n_Absorbers )         , &
              self%Absorber_Units_Name( n_Absorbers )       , &
              self%Absorber_Units_LBL( n_Absorbers )        , &
              self%Level_Pressure( 0:n_Layers )             , &
              self%Level_Temperature( 0:n_Layers )          , &
              self%Level_Absorber( 0:n_Layers, n_Absorbers ), &
              self%Level_Altitude( 0:n_Layers )             , &
              self%Layer_Pressure( n_Layers )               , &
              self%Layer_Temperature( n_Layers )            , &
              self%Layer_Absorber( n_Layers, n_Absorbers )  , &
              self%Layer_Delta_Z( n_Layers )                , &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN


    ! Initialise
    ! ...Dimensions
    self%n_Layers    = n_Layers
    self%n_Absorbers = n_Absorbers
    ! ...Arrays
    self%Absorber_ID         = 0
    self%Absorber_Name       = ''
    self%Absorber_Units_ID   = 0
    self%Absorber_Units_Name = ABSORBER_UNITS_NAME(0)
    self%Absorber_Units_LBL  = ABSORBER_UNITS_CHAR(0)
    self%Level_Pressure    = ZERO
    self%Level_Temperature = ZERO
    self%Level_Absorber    = ZERO
    self%Level_Altitude    = ZERO
    self%Layer_Pressure    = ZERO
    self%Layer_Temperature = ZERO
    self%Layer_Absorber    = ZERO
    self%Layer_Delta_Z     = ZERO


    ! Set allocation indicator
    self%Is_Allocated = .TRUE.

  END SUBROUTINE AtmProfile_Create


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       AtmProfile_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of a AtmProfile object to stdout.
!
! CALLING SEQUENCE:
!       CALL AtmProfile_Inspect( AtmProfile )
!
! INPUTS:
!       AtmProfile:    AtmProfile object to display.
!                      UNITS:      N/A
!                      TYPE:       TYPE(AtmProfile_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE AtmProfile_Inspect( self )
    TYPE(AtmProfile_type), INTENT(IN) :: self
    INTEGER :: j
    WRITE(*,'(1x,"AtmProfile OBJECT")')
    ! Release/version info
    WRITE(*,'(3x,"Release.Version :",1x,i0,".",i0)') self%Release, self%Version
    ! Dimension info
    WRITE(*,'(3x,"n_Layers        :",1x,i0)') self%n_Layers
    WRITE(*,'(3x,"n_Absorbers     :",1x,i0)') self%n_Absorbers
    IF ( .NOT. AtmProfile_Associated(self) ) RETURN
    ! Display metadata
    WRITE(*,'(5x,"Profile number    : ",i0)') self%Profile
    WRITE(*,'(5x,"Description       : ",a)') TRIM(self%Description)
    WRITE(*,'(5x,"Climatology       : ",i0)') self%Climatology_Model
    WRITE(*,'(5x,"Date (YYYY/MM/DD) : ",i4.4,"/",i2.2,"/",i2.2)') self%Year, self%Month, self%Day
    WRITE(*,'(5x,"Hour              : ",i2.2)') self%Hour
    WRITE(*,'(5x,"Latitude          : ",es13.6)') self%Latitude
    WRITE(*,'(5x,"Longitude         : ",es13.6)') self%Longitude
    WRITE(*,'(5x,"Surface altitude  : ",es13.6)') self%Surface_Altitude
    ! Level data
    WRITE(*,'(3x,"Level_Pressure:")')
    WRITE(*,'(5(1x,es13.6,:))') self%Level_Pressure
    WRITE(*,'(3x,"Level_Temperature:")')
    WRITE(*,'(5(1x,es13.6,:))') self%Level_Temperature
    WRITE(*,'(3x,"Level_Absorber:")')
    DO j = 1, self%n_Absorbers
      WRITE(*,'(5x,"Absorber: ",a,3x,"Units: ",a,3x,"LBL code: ",a)') &
              self%Absorber_Name(j), self%Absorber_Units_Name(j), self%Absorber_Units_LBL(j)
      WRITE(*,'(5(1x,es13.6,:))') self%Level_Absorber(:,j)
    END DO
    WRITE(*,'(3x,"Level_Altitude:")')
    WRITE(*,'(5(1x,es13.6,:))') self%Level_Altitude
    ! Layer data
    WRITE(*,'(3x,"Layer_Pressure:")')
    WRITE(*,'(5(1x,es13.6,:))') self%Layer_Pressure
    WRITE(*,'(3x,"Layer_Temperature:")')
    WRITE(*,'(5(1x,es13.6,:))') self%Layer_Temperature
    WRITE(*,'(3x,"Layer_Absorber:")')
    DO j = 1, self%n_Absorbers
      WRITE(*,'(5x,"Absorber: ",a,3x,"Units: ",a,3x,"LBL code: ",a)') &
              self%Absorber_Name(j), self%Absorber_Units_Name(j), self%Absorber_Units_LBL(j)
      WRITE(*,'(5(1x,es13.6,:))') self%Layer_Absorber(:,j)
    END DO
    WRITE(*,'(3x,"Layer_Delta_Z:")')
    WRITE(*,'(5(1x,es13.6,:))') self%Layer_Delta_Z
  END SUBROUTINE AtmProfile_Inspect


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       AtmProfile_ValidRelease
!
! PURPOSE:
!       Function to check the AtmProfile Release value.
!
! CALLING SEQUENCE:
!       IsValid = AtmProfile_ValidRelease( AtmProfile )
!
! INPUTS:
!       AtmProfile:    AtmProfile object for which the Release component
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       TYPE(AtmProfile_type)
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

  FUNCTION AtmProfile_ValidRelease( self ) RESULT( IsValid )
    ! Arguments
    TYPE(AtmProfile_type), INTENT(IN) :: self
    ! Function result
    LOGICAL :: IsValid
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'AtmProfile_Define::ValidRelease'
    ! Local variables
    CHARACTER(ML) :: msg

    ! Set up
    IsValid = .TRUE.


    ! Check release is not too old
    IF ( self%Release < ATMPROFILE_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("An AtmProfile data update is needed. ", &
                  &"AtmProfile release is ",i0,". Valid release is ",i0,"." )' ) &
                  self%Release, ATMPROFILE_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      RETURN
    END IF


    ! Check release is not too new
    IF ( self%Release > ATMPROFILE_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("An AtmProfile software update is needed. ", &
                  &"AtmProfile release is ",i0,". Valid release is ",i0,"." )' ) &
                  self%Release, ATMPROFILE_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      RETURN
    END IF

  END FUNCTION AtmProfile_ValidRelease


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       AtmProfile_Info
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about an AtmProfile object.
!
! CALLING SEQUENCE:
!       CALL AtmProfile_Info( AtmProfile, Info )
!
! INPUTS:
!       AtmProfile:    AtmProfile object about which info is required.
!                      UNITS:      N/A
!                      TYPE:       TYPE(AtmProfile_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Info:          String containing version and dimension information
!                      about the passed AtmProfile object.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE AtmProfile_Info( self, Info )
    ! Arguments
    TYPE(AtmProfile_type), INTENT(IN)  :: self
    CHARACTER(*),          INTENT(OUT) :: Info
    ! Parameters
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    ! Local variables
    CHARACTER(256)  :: Fmt_String
    CHARACTER(2000) :: Long_String

    ! Create the format string
    WRITE( Fmt_String,'("(a,",''" AtmProfile RELEASE.VERSION: "'',",i2,",''"."'',",i2.2,2x,", &
                      &''"N_LAYERS="'',",i0,2x,", &
                      &''"N_ABSORBERS="'',",i0,2x,",&
                      &"a,",''"     ABSORBERS:      "'', ", ", i0, "a8,", &
                      &"a,",''"     ABSORBER_UNITS: "'', ", ", i0, "a8)")' ) &
                      self%n_Absorbers, self%n_Absorbers


    ! Write the required data to the local string
    WRITE( Long_String,FMT=Fmt_String ) &
           ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
           self%Release, self%Version, &
           self%n_Layers, &
           self%n_Absorbers, &
           ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
           self%Absorber_Name, &
           ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
           self%Absorber_Units_Name

    ! Trim the output based on the
    ! dummy argument string length
    Info = Long_String(1:MIN(LEN(Info), LEN_TRIM(Long_String)))

  END SUBROUTINE AtmProfile_Info


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       AtmProfile_Absorber_Name
!
! PURPOSE:
!       Elemental subroutine to assign the absorber name component using the
!       absorber Id component of AtmProfile objects.
!
! CALLING SEQUENCE:
!       CALL AtmProfile_Absorber_Name( AtmProfile )
!
! IN/OUTPUTS:
!       AtmProfile:    AtmProfile object which is to have its absorber name
!                      component set.
!                      UNITS:      N/A
!                      TYPE:       TYPE(AtmProfile_type)
!                      DIMENSION:  Scalar or any rank.
!                      ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE AtmProfile_Absorber_Name( self )
    TYPE(AtmProfile_type), INTENT(IN OUT) :: self
    INTEGER :: j, id
    IF ( .NOT. AtmProfile_Associated(self) ) RETURN
    DO j = 1, self%n_Absorbers
      id = MAX(MIN(self%Absorber_ID(j),N_ABSORBERS), 0)
      self%Absorber_Name(j) = ABSORBER_NAME(id)
    END DO
  END SUBROUTINE AtmProfile_Absorber_Name


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       AtmProfile_Absorber_Units_Name
!
! PURPOSE:
!       Elemental subroutine to assign the absorber units name component using
!       the absorber units Id component of AtmProfile objects.
!
! CALLING SEQUENCE:
!       CALL AtmProfile_Absorber_Units_Name( AtmProfile )
!
! IN/OUTPUTS:
!       AtmProfile:    AtmProfile object which is to have its absorber units
!                      name component set.
!                      UNITS:      N/A
!                      TYPE:       TYPE(AtmProfile_type)
!                      DIMENSION:  Scalar or any rank.
!                      ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE AtmProfile_Absorber_Units_Name( self )
    TYPE(AtmProfile_type), INTENT(IN OUT) :: self
    INTEGER :: j, id
    IF ( .NOT. AtmProfile_Associated(self) ) RETURN
    DO j = 1, self%n_Absorbers
      id = MAX(MIN(self%Absorber_Units_ID(j),N_ABSORBER_UNITS), 0)
      self%Absorber_Units_Name(j) = ABSORBER_UNITS_NAME(id)
    END DO
  END SUBROUTINE AtmProfile_Absorber_Units_Name


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       AtmProfile_Absorber_Units_LBL
!
! PURPOSE:
!       Elemental subroutine to assign the absorber units LBL code component
!       using the absorber units Id component of AtmProfile objects.
!
! CALLING SEQUENCE:
!       CALL AtmProfile_Absorber_Units_LBL( AtmProfile )
!
! IN/OUTPUTS:
!       AtmProfile:    AtmProfile object which is to have its absorber units
!                      LBL code component set.
!                      UNITS:      N/A
!                      TYPE:       TYPE(AtmProfile_type)
!                      DIMENSION:  Scalar or any rank.
!                      ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE AtmProfile_Absorber_Units_LBL( self )
    TYPE(AtmProfile_type), INTENT(IN OUT) :: self
    INTEGER :: j, id
    IF ( .NOT. AtmProfile_Associated(self) ) RETURN
    DO j = 1, self%n_Absorbers
      id = MAX(MIN(self%Absorber_Units_ID(j),N_ABSORBER_UNITS), 0)
      self%Absorber_Units_LBL(j) = ABSORBER_UNITS_CHAR(id)
    END DO
  END SUBROUTINE AtmProfile_Absorber_Units_LBL


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       AtmProfile_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL AtmProfile_DefineVersion( Id )
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

  SUBROUTINE AtmProfile_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE AtmProfile_DefineVersion


!------------------------------------------------------------------------------
!
! NAME:
!       AtmProfile_Compare
!
! PURPOSE:
!       Function to test the equality of two AtmProfile objects.
!
!       This procedure is basically a copy of the AtmProfile_Equal function
!       but non-elemental to allow for informational output when a difference
!       is found between the two structures.
!
!       Used for debugging only.
!
! CALLING SEQUENCE:
!       is_equal = AtmProfile_Compare( x, y )
!
! OBJECTS:
!       x, y:          Two AtmProfile objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       TYPE(AtmProfile_type)
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

  FUNCTION AtmProfile_Compare( x, y ) RESULT( is_equal )
    TYPE(AtmProfile_type), INTENT(IN)  :: x, y
    LOGICAL :: is_equal
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'AtmProfile_Define::Compare'
    ! Local variable
    CHARACTER(ML) :: msg

    ! Set up
    is_equal = .FALSE.

    ! Check the object association status
    IF ( AtmProfile_Associated(x) .NEQV. AtmProfile_Associated(y) ) THEN
      msg = 'Object association statuses are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF


    ! Check contents
    ! ...Release/version info
    IF ( (x%Release /= y%Release) .OR. &
         (x%Version /= y%Version) ) THEN
      msg = 'Object releases/versions are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF
    ! ...Dimensions
    IF ( (x%n_Layers    /= y%n_Layers   ) .OR. &
         (x%n_Absorbers /= y%n_Absorbers) ) THEN
      msg = 'Object dimensions are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF
    ! ...Metadata
    IF ( .NOT. ((x%Profile              ==     y%Profile          ) .AND. &
                (x%Description          ==     y%Description      ) .AND. &
                (x%Climatology_Model    ==     y%Climatology_Model) .AND. &
                (x%Year                 ==     y%Year             ) .AND. &
                (x%Month                ==     y%Month            ) .AND. &
                (x%Day                  ==     y%Day              ) .AND. &
                (x%Hour                 ==     y%Hour             ) .AND. &
                (x%Latitude          .EqualTo. y%Latitude         ) .AND. &
                (x%Longitude         .EqualTo. y%Longitude        ) .AND. &
                (x%Surface_Altitude  .EqualTo. y%Surface_Altitude )) ) THEN
      msg = 'Object metadata are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF
    ! ...Arrays
    IF ( AtmProfile_Associated(x) .AND. AtmProfile_Associated(y) ) THEN
      IF ( .NOT. (ALL(x%Absorber_ID            ==     y%Absorber_ID        ) .AND. &
                  ALL(x%Absorber_Name          ==     y%Absorber_Name      ) .AND. &
                  ALL(x%Absorber_Units_ID      ==     y%Absorber_Units_ID  ) .AND. &
                  ALL(x%Absorber_Units_Name    ==     y%Absorber_Units_Name) .AND. &
                  ALL(x%Absorber_Units_LBL     ==     y%Absorber_Units_LBL ) .AND. &
                  ALL(x%Level_Pressure      .EqualTo. y%Level_Pressure     ) .AND. &
                  ALL(x%Level_Temperature   .EqualTo. y%Level_Temperature  ) .AND. &
                  ALL(x%Level_Absorber      .EqualTo. y%Level_Absorber     ) .AND. &
                  ALL(x%Level_Altitude      .EqualTo. y%Level_Altitude     ) .AND. &
                  ALL(x%Layer_Pressure      .EqualTo. y%Layer_Pressure     ) .AND. &
                  ALL(x%Layer_Temperature   .EqualTo. y%Layer_Temperature  ) .AND. &
                  ALL(x%Layer_Absorber      .EqualTo. y%Layer_Absorber     ) .AND. &
                  ALL(x%Layer_Delta_Z       .EqualTo. y%Layer_Delta_Z      )) ) THEN
        msg = 'Object data are different'
        CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
      END IF
    END IF

    ! If we get here, then...
    is_equal = .TRUE.

  END FUNCTION AtmProfile_Compare



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
!       AtmProfile_Equal
!
! PURPOSE:
!       Elemental function to test the equality of two AtmProfile objects.
!       Used in OPERATOR(==) interface block.
!
! CALLING SEQUENCE:
!       is_equal = AtmProfile_Equal( x, y )
!
!         or
!
!       IF ( x == y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:          Two AtmProfile objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       TYPE(AtmProfile_type)
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

  ELEMENTAL FUNCTION AtmProfile_Equal( x, y ) RESULT( is_equal )
    TYPE(AtmProfile_type), INTENT(IN)  :: x, y
    LOGICAL :: is_equal

    ! Set up
    is_equal = .FALSE.

    ! Check the object association status
    IF ( AtmProfile_Associated(x) .NEQV. AtmProfile_Associated(y) ) RETURN

    ! Check contents
    ! ...Release/version info
    IF ( (x%Release /= y%Release) .OR. &
         (x%Version /= y%Version) ) RETURN
    ! ...Dimensions
    IF ( (x%n_Layers    /= y%n_Layers   ) .OR. &
         (x%n_Absorbers /= y%n_Absorbers) ) RETURN
    ! ...Metadata
    IF ( .NOT. ((x%Profile              ==     y%Profile          ) .AND. &
                (x%Description          ==     y%Description      ) .AND. &
                (x%Climatology_Model    ==     y%Climatology_Model) .AND. &
                (x%Year                 ==     y%Year             ) .AND. &
                (x%Month                ==     y%Month            ) .AND. &
                (x%Day                  ==     y%Day              ) .AND. &
                (x%Hour                 ==     y%Hour             ) .AND. &
                (x%Latitude          .EqualTo. y%Latitude         ) .AND. &
                (x%Longitude         .EqualTo. y%Longitude        ) .AND. &
                (x%Surface_Altitude  .EqualTo. y%Surface_Altitude )) ) RETURN
    ! ...Arrays
    IF ( AtmProfile_Associated(x) .AND. AtmProfile_Associated(y) ) THEN
      IF ( .NOT. (ALL(x%Absorber_ID            ==     y%Absorber_ID        ) .AND. &
                  ALL(x%Absorber_Name          ==     y%Absorber_Name      ) .AND. &
                  ALL(x%Absorber_Units_ID      ==     y%Absorber_Units_ID  ) .AND. &
                  ALL(x%Absorber_Units_Name    ==     y%Absorber_Units_Name) .AND. &
                  ALL(x%Absorber_Units_LBL     ==     y%Absorber_Units_LBL ) .AND. &
                  ALL(x%Level_Pressure      .EqualTo. y%Level_Pressure     ) .AND. &
                  ALL(x%Level_Temperature   .EqualTo. y%Level_Temperature  ) .AND. &
                  ALL(x%Level_Absorber      .EqualTo. y%Level_Absorber     ) .AND. &
                  ALL(x%Level_Altitude      .EqualTo. y%Level_Altitude     ) .AND. &
                  ALL(x%Layer_Pressure      .EqualTo. y%Layer_Pressure     ) .AND. &
                  ALL(x%Layer_Temperature   .EqualTo. y%Layer_Temperature  ) .AND. &
                  ALL(x%Layer_Absorber      .EqualTo. y%Layer_Absorber     ) .AND. &
                  ALL(x%Layer_Delta_Z       .EqualTo. y%Layer_Delta_Z      )) ) RETURN
    END IF

    ! If we get here, then...
    is_equal = .TRUE.

  END FUNCTION AtmProfile_Equal


!------------------------------------------------------------------------------
!
! NAME:
!       AtmProfile_NotEqual
!
! PURPOSE:
!       Elemental function to test the inequality of two AtmProfile objects.
!       Used in OPERATOR(/=) interface block.
!
!       This function is syntactic sugar.
!
! CALLING SEQUENCE:
!       not_equal = AtmProfile_NotEqual( x, y )
!
!         or
!
!       IF ( x /= y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:          Two AtmProfile objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       AtmProfile_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       not_equal:     Logical value indicating whether the inputs are not equal.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Same as inputs.
!
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION AtmProfile_NotEqual( x, y ) RESULT( not_equal )
    TYPE(AtmProfile_type), INTENT(IN) :: x, y
    LOGICAL :: not_equal
    not_equal = .NOT. (x == y)
  END FUNCTION AtmProfile_NotEqual

END MODULE AtmProfile_Define
