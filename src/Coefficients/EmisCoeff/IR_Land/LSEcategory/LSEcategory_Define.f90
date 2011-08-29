!
! LSEcategory_Define
!
! Module defining the LSEcategory object.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 17-Aug-2011
!                       paul.vandelst@noaa.gov
 
MODULE LSEcategory_Define

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
  ! Parameters
  PUBLIC :: LSECATEGORY_DATATYPE
  ! Datatypes
  PUBLIC :: LSEcategory_type
  ! Operators
  PUBLIC :: OPERATOR(==)
  ! Procedures
  PUBLIC :: LSEcategory_Associated
  PUBLIC :: LSEcategory_Destroy
  PUBLIC :: LSEcategory_Create
  PUBLIC :: LSEcategory_Inspect
  PUBLIC :: LSEcategory_ValidRelease
  PUBLIC :: LSEcategory_Info
  PUBLIC :: LSEcategory_Name
  PUBLIC :: LSEcategory_DefineVersion
  PUBLIC :: LSEcategory_SetValue
  PUBLIC :: LSEcategory_GetValue
  PUBLIC :: LSEcategory_InquireFile
  PUBLIC :: LSEcategory_ReadFile
  PUBLIC :: LSEcategory_WriteFile


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE LSEcategory_Equal
  END INTERFACE OPERATOR(==)


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
    '$Id$'
  ! Datatype information
  CHARACTER(*), PARAMETER :: LSECATEGORY_DATATYPE = 'LSEcategory'
  ! Release and version
  INTEGER, PARAMETER :: LSECATEGORY_RELEASE = 1  ! This determines structure and file formats.
  INTEGER, PARAMETER :: LSECATEGORY_VERSION = 1  ! This is just the default data version.
  ! Close status for write errors
  CHARACTER(*), PARAMETER :: WRITE_ERROR_STATUS = 'DELETE'
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp
  ! String lengths
  INTEGER,  PARAMETER :: ML = 256 ! Message length
  INTEGER,  PARAMETER :: SL =  80 ! String length


  ! ----------------------------------
  ! LSEcategory data type definitions
  ! ----------------------------------
  !:tdoc+:
  TYPE :: LSEcategory_type
    PRIVATE
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! Datatype information
    CHARACTER(SL) :: Datatype_Name = LSECATEGORY_DATATYPE
    ! Release and version information
    INTEGER(Long) :: Release = LSECATEGORY_RELEASE
    INTEGER(Long) :: Version = LSECATEGORY_VERSION
    ! Dimensions
    INTEGER(Long) :: String_Length   = SL
    INTEGER(Long) :: n_Frequencies   = 0  ! L dim.
    INTEGER(Long) :: n_Surface_Types = 0  ! N dim.
    ! Dimensional vectors
    REAL(Double),  ALLOCATABLE :: Frequency(:)      ! Lx1
    CHARACTER(SL), ALLOCATABLE :: Surface_Type(:)   ! Nx1
    ! Reflectance LUT data
    REAL(Double),  ALLOCATABLE :: Reflectance(:,:)  ! LxN
  END TYPE LSEcategory_type
  !:tdoc-:


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                           ## PUBLIC PROCEDURES ##                          ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LSEcategory_Associated
!
! PURPOSE:
!       Elemental function to test the status of the allocatable components
!       of the LSEcategory structure.
!
! CALLING SEQUENCE:
!       Status = LSEcategory_Associated( LSEcategory )
!
! OBJECTS:
!       LSEcategory:  Structure which is to have its member's
!                      status tested.
!                      UNITS:      N/A
!                      TYPE:       LSEcategory_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:     The return value is a logical value indicating the
!                   status of the NLTE members.
!                    .TRUE.  - if ANY of the LSEcategory allocatable members
!                              are in use.
!                    .FALSE. - if ALL of the LSEcategory allocatable members
!                              are not in use.
!                   UNITS:      N/A
!                   TYPE:       LOGICAL
!                   DIMENSION:  Same as input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION LSEcategory_Associated( self ) RESULT( Status )
    TYPE(LSEcategory_type), INTENT(IN) :: self
    LOGICAL :: Status
    Status = self%Is_Allocated
  END FUNCTION LSEcategory_Associated

  
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LSEcategory_Destroy
!
! PURPOSE:
!       Elemental subroutine to re-initialize LSEcategory objects.
!
! CALLING SEQUENCE:
!       CALL LSEcategory_Destroy( LSEcategory )
!
! OBJECTS:
!       LSEcategory: Re-initialized LSEcategory structure.
!                     UNITS:      N/A
!                     TYPE:       LSEcategory_type
!                     DIMENSION:  Scalar or any rank
!                     ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE LSEcategory_Destroy( self )
    TYPE(LSEcategory_type), INTENT(OUT) :: self
    self%Is_Allocated = .FALSE.
    self%n_Frequencies   = 0
    self%n_Surface_Types = 0
  END SUBROUTINE LSEcategory_Destroy


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LSEcategory_Create
!
! PURPOSE:
!       Elemental subroutine to create an instance of an LSEcategory object.
!
! CALLING SEQUENCE:
!       CALL LSEcategory_Create( LSEcategory    , &
!                                n_Frequencies  , &     
!                                n_Surface_Types  )         
!
! OBJECTS:
!       LSEcategory:        LSEcategory object structure.
!                           UNITS:      N/A
!                           TYPE:       LSEcategory_type
!                           DIMENSION:  Scalar or any rank
!                           ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_Frequencies:      Number of spectral frequencies for which there are
!                           reflectance data.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Conformable with the LSEcategory object
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Surface_Types:    Number of land surface types for which is are
!                           reflectance data.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Conformable with the LSEcategory object
!                           ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE LSEcategory_Create( &
    self           , &  ! Output
    n_Frequencies  , &  ! Input
    n_Surface_Types  )  ! Input
    ! Arguments
    TYPE(LSEcategory_type), INTENT(OUT) :: self
    INTEGER               , INTENT(IN)  :: n_Frequencies          
    INTEGER               , INTENT(IN)  :: n_Surface_Types             
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_Frequencies   < 1 .OR. &
         n_Surface_Types < 1 ) RETURN

   
    ! Perform the allocation
    ALLOCATE( self%Frequency( n_Frequencies ), &
              self%Surface_Type( n_Surface_Types ), &
              self%Reflectance( n_Frequencies, n_Surface_Types ), &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN


    ! Initialise
    ! ...Dimensions
    self%n_Frequencies   = n_Frequencies  
    self%n_Surface_Types = n_Surface_Types
    ! ...Arrays
    self%Frequency    = ZERO
    self%Surface_Type = ''
    self%Reflectance  = ZERO

    ! Set allocation indicator
    self%Is_Allocated = .TRUE.

  END SUBROUTINE LSEcategory_Create


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LSEcategory_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of a LSEcategory object to stdout.
!
! CALLING SEQUENCE:
!       CALL LSEcategory_Inspect( LSEcategory )
!
! OBJECTS:
!       LSEcategory:  LSEcategory object to display.
!                      UNITS:      N/A
!                      TYPE:       LSEcategory_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE LSEcategory_Inspect( self)
    TYPE(LSEcategory_type), INTENT(IN) :: self
    INTEGER :: n
    WRITE(*,'(1x,"LSEcategory OBJECT")')
    ! Release/version info
    WRITE(*,'(3x,"Release.Version  :",1x,i0,".",i0)') self%Release, self%Version
    ! Dimensions
    WRITE(*,'(3x,"n_Frequencies    :",1x,i0)') self%n_Frequencies
    WRITE(*,'(3x,"n_Surface_Types  :",1x,i0)') self%n_Surface_Types
    IF ( .NOT. LSEcategory_Associated(self) ) RETURN
    ! Dimension arrays
    WRITE(*,'(3x,"Frequency :")')
    WRITE(*,'(5(1x,es13.6,:))') self%Frequency
    WRITE(*,'(3x,"Surface_Type :")')
    WRITE(*,'(4(a,:))') self%Surface_Type
    ! Reflectance array
    WRITE(*,'(3x,"Reflectance :")')
    DO n = 1, self%n_Surface_Types
      WRITE(*,'(5x,a)') self%Surface_Type(n)
      WRITE(*,'(5(1x,es13.6,:))') self%Reflectance(:,n)
    END DO
  END SUBROUTINE LSEcategory_Inspect



!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LSEcategory_ValidRelease
!
! PURPOSE:
!       Function to check the LSEcategory Release value.
!
! CALLING SEQUENCE:
!       IsValid = LSEcategory_ValidRelease( LSEcategory )
!
! INPUTS:
!       LSEcategory:  LSEcategory object for which the Release component
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       LSEcategory_type
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

  FUNCTION LSEcategory_ValidRelease( self ) RESULT( IsValid )
    ! Arguments
    TYPE(LSEcategory_type), INTENT(IN) :: self
    ! Function result
    LOGICAL :: IsValid
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'LSEcategory_ValidRelease'
    ! Local variables
    CHARACTER(ML) :: msg

    ! Set up
    IsValid = .TRUE.


    ! Check release is not too old
    IF ( self%Release < LSECATEGORY_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("An LSEcategory data update is needed. ", &
                  &"LSEcategory release is ",i0,". Valid release is ",i0,"." )' ) &
                  self%Release, LSECATEGORY_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION ); RETURN
    END IF


    ! Check release is not too new
    IF ( self%Release > LSECATEGORY_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("An LSEcategory software update is needed. ", &
                  &"LSEcategory release is ",i0,". Valid release is ",i0,"." )' ) &
                  self%Release, LSECATEGORY_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION ); RETURN
    END IF

  END FUNCTION LSEcategory_ValidRelease


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LSEcategory_Info
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about a LSEcategory object.
!
! CALLING SEQUENCE:
!       CALL LSEcategory_Info( LSEcategory, Info )
!
! OBJECTS:
!       LSEcategory:  LSEcategory object about which info is required.
!                      UNITS:      N/A
!                      TYPE:       LSEcategory_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Info:          String containing version and dimension information
!                      about the LSEcategory object.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE LSEcategory_Info( self, Info )
    ! Arguments
    TYPE(LSEcategory_type), INTENT(IN)  :: self
    CHARACTER(*),            INTENT(OUT) :: Info
    ! Parameters
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    ! Local variables
    CHARACTER(2000) :: Long_String

    ! Write the required data to the local string
    WRITE( Long_String, &
           '(a,1x,"LSEcategory RELEASE.VERSION: ",i2,".",i2.2,a,3x, &
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

  END SUBROUTINE LSEcategory_Info
 
 
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LSEcategory_Name
!
! PURPOSE:
!       Function to return the datatype name of an LSEcategory object.
!
! CALLING SEQUENCE:
!       datatype_name = LSEcategory_Name( LSEcategory )         
!
! OBJECTS:
!       LSEcategory:  LSEcategory object structure.
!                     UNITS:      N/A
!                     TYPE:       LSEcategory_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:       The return value is a the character string containing
!                     the datatype name of the structure.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER
!                     DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION LSEcategory_Name( self ) RESULT( datatype_name )
    ! Arguments
    TYPE(LSEcategory_type), INTENT(OUT) :: self
    ! Function result
    CHARACTER(LEN(self%Datatype_Name)) :: datatype_name
    
    datatype_name = self%Datatype_Name

  END FUNCTION LSEcategory_Name


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LSEcategory_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL LSEcategory_DefineVersion( Id )
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

  SUBROUTINE LSEcategory_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE LSEcategory_DefineVersion



!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LSEcategory_SetValue
!
! PURPOSE:
!       Subroutine to set the contents of a valid LSEcategory object.
!
! CALLING SEQUENCE:
!       CALL LSEcategory_SetValue( LSEcategory, &
!                                  Version      = Version     , &
!                                  Frequency    = Frequency   , &
!                                  Surface_Type = Surface_Type, &
!                                  Reflectance  = Reflectance   )
!
! OBJECTS:
!       LSEcategory:        Valid, allocated LSEcategory object for which
!                           values are to be set.
!                           UNITS:      N/A
!                           TYPE:       LSEcategory_type
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUTS:
!       Version:            Integer indicating the data version. If not specified
!                           the value of the module parameter LSECATEGORY_VERSION
!                           is used.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Frequency:          Real array to which the Frequency component of the
!                           LSEcategory object is to be set. The size of the
!                           input must match the allocated size of the component,
!                           otherwise all the component values are set to zero.
!                           UNITS:      N/A
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Rank-1 (L)
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Surface_Type:       Character array to which the Surface_Type component
!                           of the LSEcategory object is to be set. The size of the
!                           input must match the allocated size of the component,
!                           otherwise all the component values are set to a blank string.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Rank-1 (N)
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Reflectance:        Real array to which the Reflectance component of the
!                           LSEcategory object is to be set. The size of the
!                           input must match the allocated size of the component,
!                           otherwise all the component values are set to zero.
!                           UNITS:      N/A
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Rank-2 (L x N)
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE LSEcategory_SetValue( &
    self        , &  ! Input
    Version     , &  ! Optional input
    Frequency   , &  ! Optional input
    Surface_Type, &  ! Optional input
    Reflectance   )  ! Optional input
    ! Arguments
    TYPE(LSEcategory_type), INTENT(IN OUT) :: self
    INTEGER     , OPTIONAL, INTENT(IN)     :: Version
    REAL(fp)    , OPTIONAL, INTENT(IN)     :: Frequency(:)
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Surface_Type(:)
    REAL(fp)    , OPTIONAL, INTENT(IN)     :: Reflectance(:,:)
   
    IF ( .NOT. LSEcategory_Associated(self) ) RETURN

    IF ( PRESENT(Version) ) self%Version = Version
    
    IF ( PRESENT(Frequency) ) THEN
      IF ( SIZE(Frequency) == self%n_Frequencies ) THEN
        self%Frequency = Frequency
      ELSE
        self%Frequency = ZERO
      END IF
    END IF
   
    IF ( PRESENT(Surface_Type) ) THEN
      IF ( SIZE(Surface_Type) == self%n_Surface_Types ) THEN
        self%Surface_Type = Surface_Type
      ELSE
        self%Surface_Type = ''
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
   
  END SUBROUTINE LSEcategory_SetValue
 

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LSEcategory_GetValue
!
! PURPOSE:
!       Subroutine to get the contents of a valid LSEcategory object.
!
! CALLING SEQUENCE:
!       CALL LSEcategory_GetValue( LSEcategory, &
!                                  Surface_Type_ToGet  = Surface_Type_ToGet , &
!                                  Version             = Version            , &
!                                  n_Frequencies       = n_Frequencies      , &
!                                  n_Surface_Types     = n_Surface_Types    , &
!                                  Frequency           = Frequency          , &
!                                  Surface_Type        = Surface_Type       , &
!                                  Reflectance         = Reflectance        , &
!                                  Surface_Reflectance = Surface_Reflectance  )
!
! OBJECTS:
!       LSEcategory:             Valid, allocated LSEcategory object from which
!                                values are to be retrieved.
!                                UNITS:      N/A
!                                TYPE:       LSEcategory_type
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       Surface_Type_ToGet:      Character string containing a valid surface type
!                                name in the LSEcategory object.
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
!                                LSEcategory object will be assigned. The actual
!                                argument must be declared as allocatable.
!                                UNITS:      N/A
!                                TYPE:       REAL(fp)
!                                DIMENSION:  Rank-1 (L)
!                                ATTRIBUTES: INTENT(OUT), OPTIONAL, ALLOCATABLE
!
!       Surface_Type:            Character array to which the Surface_Type component
!                                of the LSEcategory object will be assigned. The actual
!                                argument must be declared as allocatable.
!                                UNITS:      N/A
!                                TYPE:       CHARACTER(*)
!                                DIMENSION:  Rank-1 (N)
!                                ATTRIBUTES: INTENT(OUT), OPTIONAL, ALLOCATABLE
!
!       Reflectance:             Real array to which the Reflectance component of the
!                                LSEcategory object will be assigned. The actual
!                                argument must be declared as allocatable.
!                                UNITS:      N/A
!                                TYPE:       REAL(fp)
!                                DIMENSION:  Rank-2 (L x N)
!                                ATTRIBUTES: INTENT(OUT), OPTIONAL, ALLOCATABLE
!
!       Surface_Reflectance:     Real array to which the Reflectance component for a
!                                given surface type in LSEcategory object will be
!                                assigned. The actual argument must be declared as
!                                allocatable.
!                                NOTE: - This argument is used in conjuction with
!                                        the Surface_Type_ToGet dummy input
!                                        argument to retrieve the reflectance of a
!                                        particular surface type.
!                                      - This argument is ignored if the optional
!                                        Surface_Type_ToGet argument is not
!                                        also provided.
!                                UNITS:      N/A
!                                TYPE:       REAL(fp)
!                                DIMENSION:  Rank-1 (L)
!                                ATTRIBUTES: INTENT(OUT), OPTIONAL, ALLOCATABLE
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE LSEcategory_GetValue( &
    self               , &  ! Input
    Surface_Type_ToGet , &  ! Optional input
    Version            , &  ! Optional output
    n_Frequencies      , &  ! Optional output
    n_Surface_Types    , &  ! Optional output
    Frequency          , &  ! Optional output
    Surface_Type       , &  ! Optional output
    Reflectance        , &  ! Optional output
    Surface_Reflectance  )  ! Optional output
    ! Arguments
    TYPE(LSEcategory_type),              INTENT(IN)  :: self
    CHARACTER(*),              OPTIONAL, INTENT(IN)  :: Surface_Type_ToGet
    INTEGER     ,              OPTIONAL, INTENT(OUT) :: Version
    INTEGER     ,              OPTIONAL, INTENT(OUT) :: n_Frequencies
    INTEGER     ,              OPTIONAL, INTENT(OUT) :: n_Surface_Types
    REAL(fp)    , ALLOCATABLE, OPTIONAL, INTENT(OUT) :: Frequency(:)
    CHARACTER(*), ALLOCATABLE, OPTIONAL, INTENT(OUT) :: Surface_Type(:)
    REAL(fp)    , ALLOCATABLE, OPTIONAL, INTENT(OUT) :: Reflectance(:,:)
    REAL(fp)    , ALLOCATABLE, OPTIONAL, INTENT(OUT) :: Surface_Reflectance(:)
    ! Local variables
    INTEGER :: i
     
    IF ( .NOT. LSEcategory_Associated(self) ) RETURN
   
    IF ( PRESENT(Version        ) ) Version         = self%Version
    IF ( PRESENT(n_Frequencies  ) ) n_Frequencies   = self%n_Frequencies
    IF ( PRESENT(n_Surface_Types) ) n_Surface_Types = self%n_Surface_Types

    IF ( PRESENT(Frequency) ) THEN
      ALLOCATE(Frequency(self%n_Frequencies))
      Frequency = self%Frequency
    END IF

    IF ( PRESENT(Surface_Type) ) THEN
      ALLOCATE(Surface_Type(self%n_Surface_Types))
      Surface_Type = self%Surface_Type
    END IF

    IF ( PRESENT(Reflectance) ) THEN
      ALLOCATE(Reflectance(self%n_Frequencies, self%n_Surface_Types))
      Reflectance = self%Reflectance
    END IF
    
    IF ( PRESENT(Surface_Type_ToGet) .AND. PRESENT(Surface_Reflectance) ) THEN
      ! Match surface type and assign
      DO i = 1, self%n_Surface_Types
        IF ( self%Surface_Type(i) == Surface_Type_ToGet ) THEN
          ALLOCATE(Surface_Reflectance(self%n_Frequencies))
          Surface_Reflectance = self%Reflectance(:,i)
        END IF
      END DO
    END IF

  END SUBROUTINE LSEcategory_GetValue
 


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LSEcategory_InquireFile
!
! PURPOSE:
!       Function to inquire LSEcategory object files.
!
! CALLING SEQUENCE:
!       Error_Status = LSEcategory_InquireFile( &
!                        Filename                         , &
!                        n_Frequencies   = n_Frequencies  , &
!                        n_Surface_Types = n_Surface_Types, &
!                        Release         = Release        , &
!                        Version         = Version          )
!
! INPUTS:
!       Filename:           Character string specifying the name of the
!                           data file to inquire.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       n_Frequencies:      Number of spectral frequencies for which there are
!                           reflectance data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Surface_Types:    Number of land surface types for which is are
!                           reflectance data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Release:            The data/file release number. Used to check
!                           for data/software mismatch.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:            The data/file version number. Used for
!                           purposes only in identifying the dataset for
!                           a particular release.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error
!                           status. The error codes are defined in the
!                           Message_Handler module.
!                           If == SUCCESS the file inquire was successful
!                              == FAILURE an unrecoverable error occurred.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION LSEcategory_InquireFile( &
    Filename       , &  ! Input
    n_Frequencies  , &  ! Optional output  
    n_Surface_Types, &  ! Optional output  
    Release        , &  ! Optional output
    Version        , &  ! Optional output
    Title          , &  ! Optional output
    History        , &  ! Optional output
    Comment        ) &  ! Optional output
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
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'LSEcategory_InquireFile'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    INTEGER :: fid
    TYPE(LSEcategory_type) :: LSEcategory

 
    ! Setup
    err_stat = SUCCESS
    ! ...Check that the file exists
    IF ( .NOT. File_Exists( Filename ) ) THEN
      msg = 'File '//TRIM(Filename)//' not found.'
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Open the file
    err_stat = Open_Binary_File( Filename, fid )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Read and check the datatype name
    err_stat = Read_Datatype( fid, LSEcategory%Datatype_name )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading Datatype_Name'
      CALL Inquire_Cleanup(); RETURN
    END IF
    IF ( TRIM(LSEcategory%Datatype_Name) /= LSECATEGORY_DATATYPE ) THEN
      msg = LSECATEGORY_DATATYPE//' datatype name check failed.'
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Read the release and version
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      LSEcategory%Release, &
      LSEcategory%Version
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading Release/Version - '//TRIM(io_msg)
      CALL Inquire_Cleanup(); RETURN
    END IF
    IF ( .NOT. LSEcategory_ValidRelease( LSEcategory ) ) THEN
      msg = 'LSEcategory Release check failed.'
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Read the dimensions
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      LSEcategory%n_Frequencies  , &
      LSEcategory%n_Surface_Types
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading dimension values from '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Read the global attributes
    err_stat = ReadGAtts_Binary_File( &
                 fid, &
                 Title   = Title  , &
                 History = History, &
                 Comment = Comment  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading global attributes'
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Close the file
    CLOSE( fid, IOSTAT=io_stat, IOMSG=io_msg )
    IF ( io_stat /= 0 ) THEN
      msg = 'Error closing '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Assign the return arguments
    IF ( PRESENT(n_Frequencies  ) ) n_Frequencies   = LSEcategory%n_Frequencies  
    IF ( PRESENT(n_Surface_Types) ) n_Surface_Types = LSEcategory%n_Surface_Types    
    IF ( PRESENT(Release        ) ) Release         = LSEcategory%Release        
    IF ( PRESENT(Version        ) ) Version         = LSEcategory%Version        
    
  CONTAINS
  
    SUBROUTINE Inquire_CleanUp()
      ! Close file if necessary
      IF ( File_Open(fid) ) THEN
        CLOSE( fid, IOSTAT=io_stat, IOMSG=io_msg )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup - '//TRIM(io_msg)
      END IF
      ! Set error status and print error message
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Inquire_CleanUp
    
  END FUNCTION LSEcategory_InquireFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LSEcategory_ReadFile
!
! PURPOSE:
!       Function to read LSEcategory object files.
!
! CALLING SEQUENCE:
!       Error_Status = LSEcategory_ReadFile( &
!                        LSEcategory        , &
!                        Filename           , &
!                        No_Close = No_Close, &
!                        Quiet    = Quiet     )
!
! OBJECTS:
!       LSEcategory:    LSEcategory object containing the data read from file.
!                       UNITS:      N/A
!                       TYPE:       LSEcategory_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       LSEcategory data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       No_Close:       Set this logical argument to *NOT* close the datafile
!                       upon exiting this routine. This option is required if
!                       the LSEcategory data is embedded within another file.
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
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS, the file read was successful
!                          == FAILURE, an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION LSEcategory_ReadFile( &
    LSEcategory, &  ! Output
    Filename   , &  ! Input
    No_Close   , &  ! Optional input
    Quiet      , &  ! Optional input
    Title      , &  ! Optional output
    History    , &  ! Optional output
    Comment    , &  ! Optional output
    Debug      ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    TYPE(LSEcategory_type), INTENT(OUT) :: LSEcategory
    CHARACTER(*),           INTENT(IN)  :: Filename
    LOGICAL,      OPTIONAL, INTENT(IN)  :: No_Close
    LOGICAL,      OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    LOGICAL,      OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'LSEcategory_ReadFile'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    LOGICAL :: close_file
    LOGICAL :: noisy
    INTEGER :: io_stat
    INTEGER :: fid
    TYPE(LSEcategory_type) :: dummy
    
    ! Setup
    err_stat = SUCCESS
    ! ...Check No_Close argument
    close_file = .TRUE.
    IF ( PRESENT(No_Close) ) close_file = .NOT. No_Close
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) THEN
      IF ( Debug ) noisy = .TRUE.
    END IF

   
    ! Check if the file is open.
    IF ( File_Open( Filename ) ) THEN
      ! ...Inquire for the logical unit number
      INQUIRE( FILE=Filename, NUMBER=fid )
      ! ...Ensure it's valid
      IF ( fid < 0 ) THEN
        msg = 'Error inquiring '//TRIM(Filename)//' for its FileID'
        CALL Read_CleanUp(); RETURN
      END IF
    ELSE
      ! ...Open the file if it exists
      IF ( File_Exists( Filename ) ) THEN
        err_stat = Open_Binary_File( Filename, fid )
        IF ( err_Stat /= SUCCESS ) THEN
          msg = 'Error opening '//TRIM(Filename)
          CALL Read_CleanUp(); RETURN
        END IF
      ELSE
        msg = 'File '//TRIM(Filename)//' not found.'
        CALL Read_CleanUp(); RETURN
      END IF
    END IF


    ! Read and check the datatype name
    err_stat = Read_Datatype( fid, dummy%Datatype_name )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading Datatype_Name'
      CALL Read_Cleanup(); RETURN
    END IF
    IF ( TRIM(dummy%Datatype_Name) /= LSECATEGORY_DATATYPE ) THEN
      msg = LSECATEGORY_DATATYPE//' datatype name check failed.'
      CALL Read_Cleanup(); RETURN
    END IF
    

    ! Read and check the release and version
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      dummy%Release, &
      dummy%Version
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading Release/Version - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF
    IF ( .NOT. LSEcategory_ValidRelease( dummy ) ) THEN
      msg = 'LSEcategory Release check failed.'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the dimensions
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      dummy%n_Frequencies  , &
      dummy%n_Surface_Types
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading data dimensions - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Allocate the object
    CALL LSEcategory_Create( &
           LSEcategory          , &
           dummy%n_Frequencies  , &        
           dummy%n_Surface_Types  )                  
    IF ( .NOT. LSEcategory_Associated( LSEcategory ) ) THEN
      msg = 'LSEcategory object allocation failed.'
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Explicitly assign the version number
    LSEcategory%Version = dummy%Version
        

    ! Read the global attributes
    err_stat = ReadGAtts_Binary_File( &
                 fid, &
                 Title   = Title  , &
                 History = History, &
                 Comment = Comment  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading global attributes'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the coefficient data
    ! ...Read the surface type names
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      LSEcategory%Surface_Type
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading surface type names - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Read the dimensional vectors
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      LSEcategory%Frequency
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading dimensional vectors - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Read the reflectance data
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      LSEcategory%Reflectance
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading reflectance data - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Close the file
    IF ( close_file ) THEN
      CLOSE( fid, IOSTAT=io_stat, IOMSG=io_msg )
      IF ( io_stat /= 0 ) THEN
        msg = 'Error closing '//TRIM(Filename)//' - '//TRIM(io_msg)
        CALL Read_Cleanup(); RETURN
      END IF
    END IF


    ! Output an info message
     IF ( noisy ) THEN
       CALL LSEcategory_Info( LSEcategory, msg )
       CALL Display_Message( ROUTINE_NAME, 'FILE: '//TRIM(Filename)//'; '//TRIM(msg), INFORMATION )
     END IF

   CONTAINS
   
     SUBROUTINE Read_CleanUp()
       IF ( File_Open(Filename) ) THEN
         CLOSE( fid, IOSTAT=io_stat, IOMSG=io_msg )
         IF ( io_stat /= 0 ) &
           msg = TRIM(msg)//'; Error closing input file during error cleanup - '//TRIM(io_msg)
       END IF
       CALL LSEcategory_Destroy( LSEcategory )
       err_stat = FAILURE
       CALL Display_Message( ROUTINE_NAME, msg, err_stat )
     END SUBROUTINE Read_CleanUp

  END FUNCTION LSEcategory_ReadFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LSEcategory_WriteFile
!
! PURPOSE:
!       Function to write LSEcategory object files.
!
! CALLING SEQUENCE:
!       Error_Status = LSEcategory_WriteFile( &
!                        LSEcategory        , &
!                        Filename           , &
!                        No_Close = No_Close, &
!                        Quiet    = Quiet     )
!
! OBJECTS:
!       LSEcategory:    LSEcategory object containing the data to write to file.
!                       UNITS:      N/A
!                       TYPE:       LSEcategory_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       LSEcategory format data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       No_Close:       Set this logical argument to *NOT* close the datafile
!                       upon exiting this routine. This option is required if
!                       the LSEcategory data is to be embedded within another file.
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
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS, the file write was successful
!                          == FAILURE, an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION LSEcategory_WriteFile( &
    LSEcategory, &  ! Input
    Filename   , &  ! Input
    No_Close   , &  ! Optional input
    Quiet      , &  ! Optional input
    Title      , &  ! Optional input
    History    , &  ! Optional input
    Comment    , &  ! Optional input
    Debug      ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    TYPE(LSEcategory_type), INTENT(IN) :: LSEcategory
    CHARACTER(*),           INTENT(IN) :: Filename
    LOGICAL,      OPTIONAL, INTENT(IN) :: No_Close
    LOGICAL,      OPTIONAL, INTENT(IN) :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
    LOGICAL,      OPTIONAL, INTENT(IN) :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'LSEcategory_WriteFile'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    LOGICAL :: close_file
    LOGICAL :: noisy
    INTEGER :: io_stat
    INTEGER :: fid
    

    ! Setup
    err_stat = SUCCESS
    ! ...Check No_Close argument
    close_file = .TRUE.
    IF ( PRESENT(No_Close) ) close_file = .NOT. No_Close
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) THEN
      IF ( Debug ) noisy = .TRUE.
    END IF
    ! ...Check there is data to write
    IF ( .NOT. LSEcategory_Associated( LSEcategory ) ) THEN
      msg = 'LSEcategory object is empty.'
      CALL Write_Cleanup(); RETURN
    END IF

   
    ! Check if the file is open.
    IF ( File_Open( FileName ) ) THEN
      ! ...Inquire for the logical unit number
      INQUIRE( FILE=Filename, NUMBER=fid )
      ! ...Ensure it's valid
      IF ( fid < 0 ) THEN
        msg = 'Error inquiring '//TRIM(Filename)//' for its FileID'
        CALL Write_CleanUp(); RETURN
      END IF
    ELSE
      ! ...Open the file for output
      err_stat = Open_Binary_File( Filename, fid, For_Output=.TRUE. )
      IF ( err_Stat /= SUCCESS ) THEN
        msg = 'Error opening '//TRIM(Filename)
        CALL Write_CleanUp(); RETURN
      END IF
    END IF


    ! Write the datatype name
    WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      LEN(LSEcategory%Datatype_Name)
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing Datatype_Name length - '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF
    WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      LSEcategory%Datatype_Name
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing Datatype_Name - '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF
    

    ! Write the release and version
    WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      LSEcategory%Release, &
      LSEcategory%Version
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing Release/Version - '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the dimensions
    WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      LSEcategory%n_Frequencies  , &
      LSEcategory%n_Surface_Types
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing data dimensions - '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the global attributes
    err_stat = WriteGAtts_Binary_File( &
                 fid, &
                 Write_Module = MODULE_VERSION_ID, &
                 Title        = Title  , &
                 History      = History, &
                 Comment      = Comment  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing global attributes'
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the coefficient data
    ! ...Write the surface type names
    WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      LSEcategory%Surface_Type
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing surface type names - '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Write the dimensional vectors
    WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      LSEcategory%Frequency
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing dimensional vectors - '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Write the reflectance data
    WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      LSEcategory%Reflectance
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing reflectance data - '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Close the file
    IF ( close_file ) THEN
      CLOSE( fid, IOSTAT=io_stat, IOMSG=io_msg )
      IF ( io_stat /= 0 ) THEN
        msg = 'Error closing '//TRIM(Filename)//' - '//TRIM(io_msg)
        CALL Write_Cleanup(); RETURN
      END IF
    END IF


    ! Output an info message
     IF ( noisy ) THEN
       CALL LSEcategory_Info( LSEcategory, msg )
       CALL Display_Message( ROUTINE_NAME, 'FILE: '//TRIM(Filename)//'; '//TRIM(msg), INFORMATION )
     END IF

   CONTAINS
   
     SUBROUTINE Write_Cleanup()
       IF ( File_Open(Filename) ) THEN
         CLOSE( fid, IOSTAT=io_stat, IOMSG=io_msg )
         IF ( io_stat /= 0 ) &
           msg = TRIM(msg)//'; Error closing output file during error cleanup - '//TRIM(io_msg)
       END IF
       err_stat = FAILURE
       CALL Display_Message( ROUTINE_NAME, msg, err_stat )
     END SUBROUTINE Write_Cleanup

  END FUNCTION LSEcategory_WriteFile

  
!################################################################################
!################################################################################
!##                                                                            ##
!##                          ## PRIVATE PROCEDURES ##                          ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!
! NAME:
!       LSEcategory_Equal
!
! PURPOSE:
!       Elemental function to test the equality of two LSEcategory objects.
!       Used in OPERATOR(==) interface block.
!
! CALLING SEQUENCE:
!       is_equal = LSEcategory_Equal( x, y )
!
!         or
!
!       IF ( x == y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:          Two LSEcategory objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       LSEcategory_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       is_equal:      Logical value indicating whether the inputs are equal.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Same as inputs.
!
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION LSEcategory_Equal( x, y ) RESULT( is_equal )
    TYPE(LSEcategory_type), INTENT(IN) :: x, y
    LOGICAL :: is_equal

    ! Set up
    is_equal = .FALSE.
   
    ! Check the object association status
    IF ( (.NOT. LSEcategory_Associated(x)) .OR. &
         (.NOT. LSEcategory_Associated(y))      ) RETURN

   ! Check contents
    ! ...Release/version info
    IF ( (x%Release /= y%Release) .OR. &
         (x%Version /= y%Version) ) RETURN
    ! ...Dimensions
    IF ( (x%n_Frequencies   /= y%n_Frequencies   ) .OR. &
         (x%n_Surface_Types /= y%n_Surface_Types ) ) RETURN
    ! ...Arrays
    IF ( ALL(x%Frequency    .EqualTo. y%Frequency    ) .AND. &
         ALL(x%Surface_Type     ==    y%Surface_Type ) .AND. &
         ALL(x%Reflectance  .EqualTo. y%Reflectance  ) ) &
      is_equal = .TRUE.

  END FUNCTION LSEcategory_Equal


  ! Function to read the datatype name from file

  FUNCTION Read_Datatype( fid, datatype_name ) RESULT( err_stat )
    ! Arguments
    INTEGER     , INTENT(IN)  :: fid
    CHARACTER(*), INTENT(OUT) :: datatype_name
    ! Function result
    INTEGER :: err_stat    
    ! Local variables
    CHARACTER(1), ALLOCATABLE :: dummy(:)
    INTEGER :: i, strlen
    INTEGER :: io_stat
    INTEGER :: alloc_stat

    ! Set up
    err_stat = FAILURE
    datatype_name = ''

    ! Get the string length
    READ( fid, IOSTAT=io_stat ) strlen
    IF ( io_stat /= 0 ) RETURN
    
    ! Allocate dummy string array
    ALLOCATE( dummy(strlen), STAT=alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN

    ! Read the string into the dummy array
    READ( fid, IOSTAT=io_stat ) dummy
    IF ( io_stat /= 0 ) RETURN

    ! Transfer array into string
    DO i = 1, MIN(strlen,LEN(datatype_name))
      datatype_name(i:i) = dummy(i)
    END DO

    ! Done
    err_stat = SUCCESS
  END FUNCTION Read_Datatype
  
END MODULE LSEcategory_Define
