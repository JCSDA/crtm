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
  ! Procedures
  PUBLIC :: AtmProfile_Associated
  PUBLIC :: AtmProfile_Destroy
  PUBLIC :: AtmProfile_Create
  PUBLIC :: AtmProfile_Inspect
  PUBLIC :: AtmProfile_ValidRelease
  PUBLIC :: AtmProfile_Info
  PUBLIC :: AtmProfile_DefineVersion  
  ! Parameters
  PUBLIC :: ATMPROFILE_N_ABSORBERS
  PUBLIC :: ATMPROFILE_N_ABSORBER_UNITS
  PUBLIC :: ATMPROFILE_ABSORBER_UNITS_ID
  PUBLIC :: ATMPROFILE_ABSORBER_UNITS_NAME
  PUBLIC :: ATMPROFILE_ABSORBER_UNITS_CHAR


  ! -------------------
  ! Procedure overloads
  ! -------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE AtmProfile_Equal
  END INTERFACE OPERATOR(==)
  

  ! -----------------
  ! Module parameters
  ! -----------------
  ! Version Id for the module
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
    '$Id$'
  ! Literal constants
  REAL(Double), PARAMETER :: ZERO = 0.0_Double
  REAL(Double), PARAMETER :: ONE  = 1.0_Double
  ! Keyword set flag
  INTEGER, PARAMETER :: SET = 1
  ! String lengths
  INTEGER, PARAMETER :: ML = 256  ! Message Length
  INTEGER, PARAMETER :: PL = 512  ! Profile description Length
  INTEGER, PARAMETER :: NL = 32   ! absorber unit Name Length
  INTEGER, PARAMETER :: LL = 1    ! absorber unit LBL Length
  ! Current valid release and version numbers
  INTEGER, PARAMETER :: ATMPROFILE_RELEASE = 2
  INTEGER, PARAMETER :: ATMPROFILE_VERSION = 1
  ! Maximum number of absorbers
  INTEGER, PARAMETER :: ATMPROFILE_N_ABSORBERS = 32
  ! Absorber units parameters
  INTEGER, PARAMETER :: ATMPROFILE_N_ABSORBER_UNITS = 8
  INTEGER :: i
  INTEGER, PARAMETER :: ATMPROFILE_ABSORBER_UNITS_ID(0:ATMPROFILE_N_ABSORBER_UNITS) = &
    (/(i,i=0,ATMPROFILE_N_ABSORBER_UNITS)/)
  CHARACTER(*), PARAMETER :: ATMPROFILE_ABSORBER_UNITS_NAME(0:ATMPROFILE_N_ABSORBER_UNITS) = &
    (/ 'Invalid', &
       'ppmv   ', &
       'cm^-3  ', &
       'g/kg   ', &
       'g.m^-3 ', &
       'hPa    ', &
       'DP, K  ', &  ! [H2O only]
       'DP, C  ', &  ! [H2O only]
       'RH, %  ' /)  ! [H2O only]
  CHARACTER(*), PARAMETER :: ATMPROFILE_ABSORBER_UNITS_CHAR(0:ATMPROFILE_N_ABSORBER_UNITS) = &
    (/ '-', &  ! Invalid
       'A', &  ! Volume mixing ratio (ppmv)
       'B', &  ! Number density (cm^-3)
       'C', &  ! Mass mixing ratio (g/kg)
       'D', &  ! Mass density (g.m^-3)
       'E', &  ! Partial pressure (hPa)
       'F', &  ! Dew point (Kelvin) [H2O only]
       'G', &  ! Dew point (Celsius) [H2O only]
       'H' /)  ! Relative humidity (%) [H2O only]


  ! --------------------------
  ! AtmProfile type definition
  ! --------------------------
  !:tdoc+:
  TYPE :: AtmProfile_type
    INTEGER :: n_Allocates = 0
    ! Release and version information
    INTEGER(Long) :: Release = ATMPROFILE_RELEASE
    INTEGER(Long) :: Version = ATMPROFILE_VERSION
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
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
!       of a AtmProfile object.
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
!       Status:      The return value is a logical value indicating the
!                    status of the AtmProfile members.
!                    .TRUE.  - if ANY of the AtmProfile allocatable or
!                              pointer members are in use.
!                    .FALSE. - if ALL of the AtmProfile allocatable or
!                              pointer members are not in use.
!                    UNITS:      N/A
!                    TYPE:       LOGICAL
!                    DIMENSION:  Same as input AtmProfile argument
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION AtmProfile_Associated( AtmProfile ) RESULT( Status )
    TYPE(AtmProfile_type), INTENT(IN) :: AtmProfile
    LOGICAL :: Status
    Status = AtmProfile%Is_Allocated
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
!                     DIMENSION:  Scalar OR any rank
!                     ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE AtmProfile_Destroy( AtmProfile )
    TYPE(AtmProfile_type), INTENT(OUT) :: AtmProfile
    AtmProfile%Is_Allocated = .FALSE.
    AtmProfile%n_Layers    = 0
    AtmProfile%n_Absorbers = 0
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
    AtmProfile , &
    n_Layers   , &
    n_Absorbers  )
    ! Arguments
    TYPE(AtmProfile_type) , INTENT(OUT) :: AtmProfile
    INTEGER,                INTENT(IN)  :: n_Layers   
    INTEGER,                INTENT(IN)  :: n_Absorbers
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'AtmProfile_Create'
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_Layers    < 1 .OR. &
         n_Absorbers < 1      ) RETURN


    ! Perform the allocation.
    ALLOCATE( AtmProfile%Absorber_ID(1:n_Absorbers)              , &
              AtmProfile%Absorber_Units_ID(1:n_Absorbers)        , &
              AtmProfile%Absorber_Units_Name(1:n_Absorbers)      , &
              AtmProfile%Absorber_Units_LBL(1:n_Absorbers)       , &
              AtmProfile%Level_Pressure(0:n_Layers)              , &
              AtmProfile%Level_Temperature(0:n_Layers)           , &
              AtmProfile%Level_Absorber(0:n_Layers,1:n_Absorbers), &
              AtmProfile%Level_Altitude(0:n_Layers)              , &
              AtmProfile%Layer_Pressure(1:n_Layers)              , &
              AtmProfile%Layer_Temperature(1:n_Layers)           , &
              AtmProfile%Layer_Absorber(1:n_Layers,1:n_Absorbers), &
              AtmProfile%Layer_Delta_Z(1:n_Layers)               , &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN


    ! Initialise
    ! ...Dimensions
    AtmProfile%n_Layers    = n_Layers
    AtmProfile%n_Absorbers = n_Absorbers
    ! ...Arrays
    AtmProfile%Absorber_ID           = 0
    AtmProfile%Absorber_Units_ID     = 0
    AtmProfile%Absorber_Units_Name   = ATMPROFILE_ABSORBER_UNITS_NAME(0)
    AtmProfile%Absorber_Units_LBL    = ATMPROFILE_ABSORBER_UNITS_CHAR(0)
    
    AtmProfile%Level_Pressure    = ZERO
    AtmProfile%Level_Temperature = ZERO
    AtmProfile%Level_Absorber    = ZERO
    AtmProfile%Level_Altitude    = ZERO
    AtmProfile%Layer_Pressure    = ZERO
    AtmProfile%Layer_Temperature = ZERO
    AtmProfile%Layer_Absorber    = ZERO
    AtmProfile%Layer_Delta_Z     = ZERO


    ! Set allocation indicator
    AtmProfile%Is_Allocated = .TRUE.

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

  SUBROUTINE AtmProfile_Inspect( AtmProfile )
    TYPE(AtmProfile_type), INTENT(IN) :: AtmProfile
    INTEGER :: j
    WRITE(*,'(1x,"AtmProfile OBJECT")')
    ! Dimension info
    WRITE(*,'(3x,"n_Layers   :",1x,i0)') AtmProfile%n_Layers   
    WRITE(*,'(3x,"n_Absorbers:",1x,i0)') AtmProfile%n_Absorbers
    IF ( .NOT. AtmProfile_Associated(AtmProfile) ) RETURN
    ! Display metadata
    WRITE(*,'(5x,"Description     : ",a)') TRIM(AtmProfile%Description)
    WRITE(*,'(5x,"Climatology     : ",i0)') AtmProfile%Climatology_Model
    WRITE(*,'(5x,"Date (YYYYMMDD) : ",i4,i2.2,i2.2)') AtmProfile%Year, &
                                                      AtmProfile%Month, &
                                                      AtmProfile%Day
    WRITE(*,'(5x,"Latitude        : ",es13.6)') AtmProfile%Latitude
    WRITE(*,'(5x,"Longitude       : ",es13.6)') AtmProfile%Longitude
    WRITE(*,'(5x,"Surface altitude: ",es13.6)') AtmProfile%Surface_Altitude
    ! Level data
    WRITE(*,'(3x,"Level_Pressure:")') 
    WRITE(*,'(5(1x,es13.6,:))') AtmProfile%Level_Pressure
    WRITE(*,'(3x,"Level_Temperature:")') 
    WRITE(*,'(5(1x,es13.6,:))') AtmProfile%Level_Temperature
    WRITE(*,'(3x,"Level_Absorber:")')
    DO j = 1, AtmProfile%n_Absorbers 
      WRITE(*,'(5x,"Absorber Id: ",i0)') AtmProfile%Absorber_Id(j)
      WRITE(*,'(5(1x,es13.6,:))') AtmProfile%Level_Absorber(:,j)
    END DO
    WRITE(*,'(3x,"Level_Altitude:")') 
    WRITE(*,'(5(1x,es13.6,:))') AtmProfile%Level_Altitude
    ! Layer data
    WRITE(*,'(3x,"Layer_Pressure:")') 
    WRITE(*,'(5(1x,es13.6,:))') AtmProfile%Layer_Pressure
    WRITE(*,'(3x,"Layer_Temperature:")') 
    WRITE(*,'(5(1x,es13.6,:))') AtmProfile%Layer_Temperature
    WRITE(*,'(3x,"Layer_Absorber:")') 
    DO j = 1, AtmProfile%n_Absorbers 
      WRITE(*,'(5x,"Absorber Id: ",i0)') AtmProfile%Absorber_Id(j)
      WRITE(*,'(5(1x,es13.6,:))') AtmProfile%Layer_Absorber(:,j)
    END DO
    WRITE(*,'(3x,"Layer_Delta_Z:")') 
    WRITE(*,'(5(1x,es13.6,:))') AtmProfile%Layer_Delta_Z
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

  FUNCTION AtmProfile_ValidRelease( AtmProfile ) RESULT( IsValid )
    ! Arguments
    TYPE(AtmProfile_type), INTENT(IN) :: AtmProfile
    ! Function result
    LOGICAL :: IsValid
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'AtmProfile_ValidRelease'
    ! Local variables
    CHARACTER(ML) :: msg

    ! Set up
    IsValid = .TRUE.


    ! Check release is not too old
    IF ( AtmProfile%Release < ATMPROFILE_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("A AtmProfile data update is needed. ", &
                  &"AtmProfile release is ",i0, &
                  &". Valid release is ",i0,"." )' ) &
                  AtmProfile%Release, ATMPROFILE_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      RETURN
    END IF


    ! Check release is not too new
    IF ( AtmProfile%Release > ATMPROFILE_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("A AtmProfile software update is needed. ", &
                  &"AtmProfile release is ",i0, &
                  &". Valid release is ",i0,"." )' ) &
                  AtmProfile%Release, ATMPROFILE_RELEASE
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
!       information about a AtmProfile object.
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

  SUBROUTINE AtmProfile_Info( AtmProfile, Info )
    ! Arguments
    TYPE(AtmProfile_type), INTENT(IN)  :: AtmProfile
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
                      &"a,",''"     ABSORBER_IDs:   "'', ", ", i0, "i3,", &
                      &"a,",''"     ABSORBER_UNITS: "'', ", ", i0, "a8)")' ) &
                      AtmProfile%n_Absorbers, AtmProfile%n_Absorbers


    ! Write the required data to the local string
    WRITE( Long_String,FMT=Fmt_String ) ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                                        AtmProfile%Release, AtmProfile%Version, &
                                        AtmProfile%n_Layers, &
                                        AtmProfile%n_Absorbers, &
                                        ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                                        AtmProfile%Absorber_ID, &
                                        ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                                        AtmProfile%Absorber_Units_Name

    ! Trim the output based on the
    ! dummy argument string length
    Info = Long_String(1:MIN(LEN(Info), LEN_TRIM(Long_String)))

  END SUBROUTINE AtmProfile_Info
  
  
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
    IF ( (.NOT. AtmProfile_Associated(x)) .OR. &
         (.NOT. AtmProfile_Associated(y))      ) RETURN

    ! Check contents
    ! ...Dimensions
    IF ( (x%n_Layers    /= y%n_Layers   ) .OR. &
         (x%n_Absorbers /= y%n_Absorbers) ) RETURN
    ! ...Data
    IF ( (x%Profile              ==     y%Profile          ) .AND. &
         (x%Description          ==     y%Description      ) .AND. &
         (x%Climatology_Model    ==     y%Climatology_Model) .AND. &
         (x%Year                 ==     y%Year             ) .AND. &
         (x%Month                ==     y%Month            ) .AND. &
         (x%Day                  ==     y%Day              ) .AND. &
         (x%Hour                 ==     y%Hour             ) .AND. &
         (x%Latitude          .EqualTo. y%Latitude         ) .AND. &
         (x%Longitude         .EqualTo. y%Longitude        ) .AND. &
         (x%Surface_Altitude  .EqualTo. y%Surface_Altitude ) .AND. &
         ALL(x%Absorber_ID            ==     y%Absorber_ID        ) .AND. &
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
         ALL(x%Layer_Delta_Z       .EqualTo. y%Layer_Delta_Z      )       ) &
      is_equal = .TRUE.

  END FUNCTION AtmProfile_Equal
  
END MODULE AtmProfile_Define
