!
! CompTest_Define
!
! Module defining the structures to hold component model
! (e.g. Forward/Tangent-Linear, Tangent-Linear/Adjoint, Adjoint/K-matrix)
! test results and containing routines to manipulate it.
!       
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 03-Aug-2010
!                       paul.vandelst@noaa.gov
!

MODULE CompTest_Define

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds           , ONLY: fp
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
  PUBLIC :: CompTest_type
  ! Operators
  PUBLIC :: OPERATOR(==)
  ! Procedures
  PUBLIC :: CompTest_Associated
  PUBLIC :: CompTest_Destroy
  PUBLIC :: CompTest_Create
  PUBLIC :: CompTest_Inspect
  PUBLIC :: CompTest_Release_IsValid
  PUBLIC :: CompTest_Info
  PUBLIC :: CompTest_DefineVersion
  ! ...Test type helper procedures
  PUBLIC :: CompTest_Is_Valid
  PUBLIC :: CompTest_Is_FWDTL
  PUBLIC :: CompTest_Is_TLAD
  PUBLIC :: CompTest_Is_ADK
  PUBLIC :: CompTest_Set_FWDTL
  PUBLIC :: CompTest_Set_TLAD
  PUBLIC :: CompTest_Set_ADK
  ! ...Spectral dimension type helper procedures
  PUBLIC :: CompTest_Is_Channel
  PUBLIC :: CompTest_Is_Frequency
  ! ...Flags component helper procedures
  PUBLIC :: CompTest_HasPressure
  PUBLIC :: CompTest_HasSpectral
  PUBLIC :: CompTest_HasPerturbation
  PUBLIC :: CompTest_SetPressureFlag
  PUBLIC :: CompTest_SetSpectralFlag
  PUBLIC :: CompTest_SetPerturbationFlag  
  PUBLIC :: CompTest_ClearPressureFlag
  PUBLIC :: CompTest_ClearSpectralFlag
  PUBLIC :: CompTest_ClearPerturbationFlag  

  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE CompTest_Equal
  END INTERFACE OPERATOR(==)


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Version Id for the module
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! Current valid release and version numbers
  INTEGER, PARAMETER :: COMPTEST_RELEASE = 2  ! This determines structure and file formats.
  INTEGER, PARAMETER :: COMPTEST_VERSION = 1  ! This is just the data version for the release.
  ! String lengths
  INTEGER, PARAMETER :: SL = 20  ! Sensor id string length
  INTEGER, PARAMETER :: VL = 64  ! Variable name string length
  INTEGER, PARAMETER :: ML = 256
  ! Literals
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  ! Bit positions of the flags
  ! ...Dimensionality indicators
  INTEGER, PARAMETER :: HAS_PRESSURE_FLAG     = 0  ! 0==no , 1==yes
  INTEGER, PARAMETER :: HAS_SPECTRAL_FLAG     = 1  ! 0==no , 1==yes
  INTEGER, PARAMETER :: HAS_PERTURBATION_FLAG = 2  ! 0==no , 1==yes
  ! ...Spectral type indicator
  INTEGER, PARAMETER :: FREQUENCY_FLAG = 4  ! 0==channel , 1==frequency
  ! ...Test type indicator
  INTEGER, PARAMETER :: FWDTL_FLAG = 8   ! 0==no , 1==yes
  INTEGER, PARAMETER :: TLAD_FLAG  = 9   ! 0==no , 1==yes
  INTEGER, PARAMETER :: ADK_FLAG   = 10  ! 0==no , 1==yes


  ! -----------------------------
  ! CompTest data type definition
  ! -----------------------------
  !:tdoc+:
  TYPE :: CompTest_type
    ! Release and version information
    INTEGER :: Release = COMPTEST_RELEASE
    INTEGER :: Version = COMPTEST_VERSION
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! Dimensions
    INTEGER :: nK  = 0  ! Profile layer dimension  (optional - set to 1 for non-profile tests)
    INTEGER :: nL  = 0  ! Spectral dimension       (optional - set to 1 for non-spectral tests)
    INTEGER :: nP  = 0  ! Perturbation dimension   (optional - set to 1 for non-perturbation tests)
    INTEGER :: nIV = 0  ! Input variable dimension
    INTEGER :: nOV = 0  ! Output variable dimension
    ! The instance for a given atmospheric profile (dataset) and its identifier
    INTEGER :: nM = 0
    CHARACTER(VL) :: nM_Name = ''
    ! Bitflags
    INTEGER :: Flags    = 0
    ! Pressure, spectral, and perturbation dimension vectors
    REAL(fp), ALLOCATABLE :: Pressure(:)     ! nK
    REAL(fp), ALLOCATABLE :: Spectral(:)     ! nL
    REAL(fp), ALLOCATABLE :: Perturbation(:) ! nP
    ! The input variable names and units
    CHARACTER(VL), ALLOCATABLE :: Input_Variable_Name(:)   ! nIV
    CHARACTER(VL), ALLOCATABLE :: Input_Variable_Units(:)  ! nIV
    ! The output variable names and units
    CHARACTER(VL), ALLOCATABLE :: Output_Variable_Name(:)   ! nOV
    CHARACTER(VL), ALLOCATABLE :: Output_Variable_Units(:)  ! nOV
    ! The test results
    REAL(fp), ALLOCATABLE :: d1(:,:,:,:,:)  ! nK x nL x nP x nIV x nOV
    REAL(fp), ALLOCATABLE :: d2(:,:,:,:,:)  ! nK x nL x nP x nIV x nOV
  END TYPE CompTest_type
  !:tdoc-:


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

  ! Flag component helper procedures
  ! --------------------------------
  ELEMENTAL FUNCTION CompTest_IsFlagSet(self, flag) RESULT(Is_Set)
    TYPE(CompTest_type), INTENT(IN) :: self
    INTEGER            , INTENT(IN) :: flag
    LOGICAL :: Is_Set
    Is_Set = BTEST(self%Flags,flag)
  END FUNCTION CompTest_IsFlagSet

 
  ELEMENTAL FUNCTION CompTest_HasPressure(CompTest) RESULT(HasPressure)
    TYPE(CompTest_type), INTENT(IN) :: CompTest
    LOGICAL :: HasPressure
    HasPressure = CompTest_IsFlagSet(CompTest,HAS_PRESSURE_FLAG)
  END FUNCTION CompTest_HasPressure

  ELEMENTAL FUNCTION CompTest_HasSpectral(CompTest) RESULT(HasSpectral)
    TYPE(CompTest_type), INTENT(IN) :: CompTest
    LOGICAL :: HasSpectral
    HasSpectral = CompTest_IsFlagSet(CompTest,HAS_SPECTRAL_FLAG)
  END FUNCTION CompTest_HasSpectral

  ELEMENTAL FUNCTION CompTest_HasPerturbation(CompTest) RESULT(HasPerturbation)
    TYPE(CompTest_type), INTENT(IN) :: CompTest
    LOGICAL :: HasPerturbation
    HasPerturbation = CompTest_IsFlagSet(CompTest,HAS_PERTURBATION_FLAG)
  END FUNCTION CompTest_HasPerturbation

 
 
 
  ELEMENTAL SUBROUTINE CompTest_SetFlag(self, flag)
    TYPE(CompTest_type), INTENT(IN OUT) :: self
    INTEGER            , INTENT(IN)     :: flag
    self%Flags = IBSET(self%Flags,flag)
  END SUBROUTINE CompTest_SetFlag
 
  ELEMENTAL SUBROUTINE CompTest_SetPressureFlag(CompTest)
    TYPE(CompTest_type), INTENT(IN OUT) :: CompTest
    CALL CompTest_SetFlag(CompTest, HAS_PRESSURE_FLAG)
  END SUBROUTINE CompTest_SetPressureFlag
 
  ELEMENTAL SUBROUTINE CompTest_SetSpectralFlag(CompTest)
    TYPE(CompTest_type), INTENT(IN OUT) :: CompTest
    CALL CompTest_SetFlag(CompTest, HAS_SPECTRAL_FLAG)
  END SUBROUTINE CompTest_SetSpectralFlag
 
  ELEMENTAL SUBROUTINE CompTest_SetPerturbationFlag(CompTest)
    TYPE(CompTest_type), INTENT(IN OUT) :: CompTest
    CALL CompTest_SetFlag(CompTest, HAS_PERTURBATION_FLAG)
  END SUBROUTINE CompTest_SetPerturbationFlag
 


  ELEMENTAL SUBROUTINE CompTest_ClearFlag(self, flag)
    TYPE(CompTest_type), INTENT(IN OUT) :: self
    INTEGER            , INTENT(IN)     :: flag
    self%Flags = IBCLR(self%Flags,flag)
  END SUBROUTINE CompTest_ClearFlag
 
  ELEMENTAL SUBROUTINE CompTest_ClearPressureFlag(CompTest)
    TYPE(CompTest_type), INTENT(IN OUT) :: CompTest
    CALL CompTest_ClearFlag(CompTest, HAS_PRESSURE_FLAG)
  END SUBROUTINE CompTest_ClearPressureFlag
 
  ELEMENTAL SUBROUTINE CompTest_ClearSpectralFlag(CompTest)
    TYPE(CompTest_type), INTENT(IN OUT) :: CompTest
    CALL CompTest_ClearFlag(CompTest, HAS_Spectral_FLAG)
  END SUBROUTINE CompTest_ClearSpectralFlag
 
  ELEMENTAL SUBROUTINE CompTest_ClearPerturbationFlag(CompTest)
    TYPE(CompTest_type), INTENT(IN OUT) :: CompTest
    CALL CompTest_ClearFlag(CompTest, HAS_PERTURBATION_FLAG)
  END SUBROUTINE CompTest_ClearPerturbationFlag
 
 
  ELEMENTAL SUBROUTINE CompTest_ResetFlags(self)
    TYPE(CompTest_type), INTENT(IN OUT) :: self
    self%Flags = 0
  END SUBROUTINE CompTest_ResetFlags


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CompTest_Associated
!
! PURPOSE:
!       Elemental function to test the status of the allocatable components
!       of a CompTest object.
!
! CALLING SEQUENCE:
!       Status = CompTest_Associated( CompTest )
!
! OBJECTS:
!       CompTest:      CompTest object which is to have its member's
!                      status tested.
!                      UNITS:      N/A
!                      TYPE:       TYPE(CompTest_type)
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:        The return value is a logical value indicating the
!                      status of the CompTest members.
!                      .TRUE.  - if ANY of the CompTest allocatable or
!                                pointer members are in use.
!                      .FALSE. - if ALL of the CompTest allocatable or
!                                pointer members are not in use.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Same as input CompTest argument
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CompTest_Associated( CTest ) RESULT( Status )
    TYPE(CompTest_type), INTENT(IN) :: CTest
    LOGICAL :: Status
    Status = CTest%Is_Allocated
  END FUNCTION CompTest_Associated


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CompTest_Destroy
! 
! PURPOSE:
!       Elemental subroutine to re-initialize CompTest objects.
!
! CALLING SEQUENCE:
!       CALL CompTest_Destroy( CompTest )
!
! OBJECTS:
!       CompTest:       Re-initialized CompTest object.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CompTest_type)
!                       DIMENSION:  Scalar OR any rank
!                       ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CompTest_Destroy( CompTest )
    TYPE(CompTest_type), INTENT(OUT) :: CompTest
    CompTest%Is_Allocated = .FALSE.
    CALL CompTest_ResetFlags(CompTest)
  END SUBROUTINE CompTest_Destroy
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CompTest_Create
! 
! PURPOSE:
!       Elemental subroutine to create an instance of a CompTest object.
!
! CALLING SEQUENCE:
!       CALL CompTest_Create( CompTest, &
!                             n_InputVars    , &
!                             n_OutputVars   , &
!                             n_Layers        = n_Layers       , &
!                             n_Spectral      = n_Spectral     , &
!                             n_Perturbations = n_Perturbations, &
!                             Is_Frequency    = Is_Frequency   , &
!                             Is_ADK          = Is_ADK           )
!
! OBJECTS:
!       CompTest:          CompTest object.
!                          UNITS:      N/A
!                          TYPE:       TYPE(CompTest_type)
!                          DIMENSION:  Scalar or any rank
!                          ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_InputVars:       The number of input variables dimension of the
!                          CompTest data. Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       n_OutputVars:      The number of output variables dimension of the
!                          CompTest data. Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       n_Layers:          The size of the layer (vertical/pressure) dimension
!                          of the CompTest data. Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       n_Spectral:        The size of the spectral dimension of the CompTest
!                          data. Must be > 0.
!                          The "units" of this dimension are assumed to be
!                          SENSOR CHANNEL unless the IS_FREQUENCY optional
!                          argument is used and set.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       n_Perturbations:   The number of perturbations dimension of the
!                          CompTest data.
!                          Must be > 0.
!                          NOTE: - Setting this dimension means the component
!                                  test type is a Forward/Tangent-linear model
!                                  model comparison.
!                                - NOT setting this dimension means the component
!                                  test type is a Tangent-linear/Adjoint model
!                                  comparison, unless the IS_ADK argument is also
!                                  set.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Is_Frequency:      Logical argument to specify that the spectral
!                          dimension is frequency. If not specified, the
!                          default spectral dimension is sensor channel.
!                          If .TRUE.  == Spectral dimension is frequency.
!                             .FALSE. == Spectral dimension is spectral channel
!                          This argument is ignored if the N_SPECTRAL argument
!                          is not specified.
!                          UNITS:      N/A
!                          TYPE:       LOGICAL
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Is_ADK:            Logical argument to specify that the test type
!                          is a Adjoint/K-Matrix model comparison.
!                          NOTE: This argument is ignored if the n_Perturbations
!                                argument is specified.
!                          UNITS:      N/A
!                          TYPE:       LOGICAL
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN), OPTIONAL
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CompTest_Create( &
    CTest          , &
    n_InputVars    , &
    n_OutputVars   , &
    n_Layers       , &
    n_Spectral     , &
    n_Perturbations, &
    Is_Frequency   , &
    Is_ADK           )
    ! Arguments
    TYPE(CompTest_type), INTENT(OUT) :: CTest
    INTEGER,             INTENT(IN)  :: n_InputVars    
    INTEGER,             INTENT(IN)  :: n_OutputVars   
    INTEGER, OPTIONAL  , INTENT(IN)  :: n_Layers       
    INTEGER, OPTIONAL  , INTENT(IN)  :: n_Spectral  
    INTEGER, OPTIONAL  , INTENT(IN)  :: n_Perturbations
    LOGICAL, OPTIONAL  , INTENT(IN)  :: Is_Frequency
    LOGICAL, OPTIONAL  , INTENT(IN)  :: Is_ADK  
    ! Local variables
    INTEGER :: nK, nL, nP
    INTEGER :: alloc_stat
    LOGICAL :: Has_Pressure, Has_Spectral, Has_Perturbation
    LOGICAL :: Set_Frequency
    INTEGER :: Test_Flag
    
    ! Check input
    ! ...Double check
    CALL CompTest_Destroy(CTest)
    ! ...Set optional layer dimension
    IF ( PRESENT(n_Layers) ) THEN
      nK = n_Layers
      Has_Pressure = .TRUE.
    ELSE
      nK = 1
      Has_Pressure = .FALSE.
    END IF
    ! ...Set optional spectral dimension
    IF ( PRESENT(n_Spectral) ) THEN
      nL = n_Spectral
      Has_Spectral = .TRUE.
    ELSE
      nL = 1
      Has_Spectral = .FALSE.
    END IF
    ! ...Set optional perturbation dimension
    IF ( PRESENT(n_Perturbations) ) THEN
      nP = n_Perturbations
      Has_Perturbation = .TRUE.
      Test_Flag = FWDTL_FLAG
    ELSE
      nP = 1
      Has_Perturbation = .FALSE.
      Test_Flag = TLAD_FLAG
      IF ( PRESENT(Is_ADK) ) THEN
        IF ( Is_ADK ) Test_Flag = ADK_FLAG
      END IF
    END IF
    ! ...Check dimension values
    IF ( n_InputVars   < 1 .OR. &
         n_OutputVars  < 1 .OR. & 
         nK            < 1 .OR. &
         nL            < 1 .OR. &
         nP            < 1 ) RETURN
    ! ...Check spectral dimension "units". Channel by default
    Set_Frequency = .FALSE.
    IF ( Has_Spectral ) THEN
      IF ( PRESENT(Is_Frequency) ) Set_Frequency = Is_Frequency
    END IF


    ! Perform the allocations.
    ALLOCATE( CTest%Pressure(1:nK)    , &
              CTest%Spectral(1:nL)    , &
              CTest%Perturbation(1:nP), &
              CTest%Input_Variable_Name(1:n_InputVars)   , &
              CTest%Input_Variable_Units(1:n_InputVars)  , &
              CTest%Output_Variable_Name(1:n_OutputVars) , &
              CTest%Output_Variable_Units(1:n_OutputVars), &
              CTest%d1(1:nK, &
                       1:nL, &
                       1:nP, &
                       1:n_InputVars, &
                       1:n_OutputVars ), &
              CTest%d2(1:nK, &
                       1:nL, &
                       1:nP, &
                       1:n_InputVars, &
                       1:n_OutputVars ), &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN


    ! Initialise
    ! ...Dimensions
    CTest%nK  = nK
    CTest%nL  = nL
    CTest%nP  = nP
    CTest%nIV = n_InputVars
    CTest%nOV = n_OutputVars
    ! ...Set the bit flags
    IF ( Has_Pressure     ) CALL CompTest_SetFlag(CTest, HAS_PRESSURE_FLAG)
    IF ( Has_Spectral     ) CALL CompTest_SetFlag(CTest, HAS_SPECTRAL_FLAG)
    IF ( Has_Perturbation ) CALL CompTest_SetFlag(CTest, HAS_PERTURBATION_FLAG)
    IF ( Set_Frequency ) CALL CompTest_SetFlag(CTest, FREQUENCY_FLAG)
    CALL CompTest_SetFlag(CTest, Test_Flag)
    ! ...Arrays
    CTest%Pressure              = ZERO
    CTest%Spectral              = ZERO
    CTest%Perturbation          = ZERO
    CTest%Input_Variable_Name   = ''
    CTest%Input_Variable_Units  = ''
    CTest%Output_Variable_Name  = ''
    CTest%Output_Variable_Units = ''
    CTest%d1                    = ZERO
    CTest%d2                    = ZERO


    ! Set allocation indicator
    CTest%Is_Allocated = .TRUE.

  END SUBROUTINE CompTest_Create


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CompTest_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of a CompTest object to stdout.
!
! CALLING SEQUENCE:
!       CALL CompTest_Inspect( CompTest )
!
! INPUTS:
!       CompTest:      CompTest object to display.
!                      UNITS:      N/A
!                      TYPE:       TYPE(CompTest_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CompTest_Inspect( CTest )
    TYPE(CompTest_type), INTENT(IN) :: CTest
    INTEGER :: i
    
    WRITE(*,'(1x,"CompTest OBJECT")')

    ! Dimensions
    IF ( CompTest_IsFlagSet(CTest, HAS_PRESSURE_FLAG) ) &
      WRITE(*,'(3x,"n_Layers           :",1x,i0)') CTest%nK
    IF ( CompTest_IsFlagSet(CTest, HAS_SPECTRAL_FLAG) ) &
      WRITE(*,'(3x,"n_",a,6x,":",1x,i0)') CompTest_SpectralName(CTest), CTest%nL
    IF ( CompTest_IsFlagSet(CTest, HAS_PERTURBATION_FLAG) ) &
      WRITE(*,'(3x,"n_Perturbations    :",1x,i0)') CTest%nP
    WRITE(*,'(3x,"n_Input_Variables  :",1x,i0)') CTest%nIV
    WRITE(*,'(3x,"n_Output_Variables :",1x,i0)') CTest%nOV

    ! Scalars
    WRITE(*,'(3x,"Dataset number     :",1x,i0)') CTest%nM
    WRITE(*,'(3x,"Dataset name       :",1x,a)') TRIM(CTest%nM_Name)
    WRITE(*,'(3x,"Flags              :",1x,b31.31," (",i0,")")') CTest%Flags, CTest%Flags
    WRITE(*,'(3x,"Test type          :",1x,a)') TRIM(CompTest_TestName(CTest))

    ! Dimension vectors
    IF ( .NOT. CompTest_Associated(CTest) ) RETURN
    IF ( CompTest_IsFlagSet(CTest, HAS_PRESSURE_FLAG) ) THEN
      WRITE(*,'(3x,"CompTest Pressure:")') 
      WRITE(*,'(5(1x,es13.6,:))') CTest%Pressure
    END IF
    IF ( CompTest_IsFlagSet(CTest, HAS_SPECTRAL_FLAG) ) THEN
      WRITE(*,'(3x,"CompTest ",a," :")') TRIM(CompTest_SpectralName(CTest))
      IF ( CompTest_IsFlagSet(CTest, FREQUENCY_FLAG) ) THEN
        WRITE(*,'(5(1x,es13.6,:))') CTest%Spectral
      ELSE
        WRITE(*,'(8(1x,i5,:))') INT(CTest%Spectral)
      END IF
    END IF    
    IF ( CompTest_IsFlagSet(CTest, HAS_PERTURBATION_FLAG) ) THEN
      WRITE(*,'(3x,"CompTest Perturbation :")') 
      WRITE(*,'(5(1x,es13.6,:))') CTest%Perturbation
    END IF
    WRITE(*,'(3x,"CompTest InputVar Name/Units :")')
    DO i = 1, CTest%nIV
      WRITE(*,'(1x,a,1x,"(",a,")")') TRIM(CTest%Input_Variable_Name(i)), &
                                     TRIM(CTest%Input_Variable_Units(i))
    END DO
    WRITE(*,'(3x,"CompTest OutputVar Name/Units :")')
    DO i = 1, CTest%nOV
      WRITE(*,'(1x,a,1x,"(",a,")")') TRIM(CTest%Output_Variable_Name(i)), &
                                     TRIM(CTest%Output_Variable_Units(i))
    END DO

    ! Data arrays
    WRITE(*,'(3x,"CompTest ",a," :")') CompTest_d1Name(CTest)
    WRITE(*,'(5(1x,es13.6,:))') CTest%d1
    WRITE(*,'(3x,"CompTest ",a," :")') CompTest_d2Name(CTest)
    WRITE(*,'(5(1x,es13.6,:))') CTest%d2
    
  END SUBROUTINE CompTest_Inspect


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CompTest_Release_IsValid
!
! PURPOSE:
!       Function to check the CompTest Release value.
!
! CALLING SEQUENCE:
!       IsValid = CompTest_Release_IsValid( CompTest )
!
! INPUTS:
!       CompTest:      CompTest object for which the Release component
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       TYPE(CompTest_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       IsValid:       Logical value defining the release validity.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Scalar
!:sdoc-:
!----------------------------------------------------------------------------------

  FUNCTION CompTest_Release_IsValid( CompTest ) RESULT( IsValid )
    ! Arguments
    TYPE(CompTest_type), INTENT(IN) :: CompTest
    ! Function result
    LOGICAL :: IsValid
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CompTest_Release_IsValid'
    ! Local variables
    CHARACTER(ML) :: msg

    ! Set up
    IsValid = .TRUE.


    ! Check release is not too old
    IF ( CompTest%Release < COMPTEST_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("A CompTest data update is needed. ", &
                  &"CompTest release is ",i0, &
                  &". Valid release is ",i0,"." )' ) &
                  CompTest%Release, COMPTEST_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      RETURN
    END IF


    ! Check release is not too new
    IF ( CompTest%Release > COMPTEST_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("A CompTest software update is needed. ", &
                  &"CompTest release is ",i0, &
                  &". Valid release is ",i0,"." )' ) &
                  CompTest%Release, COMPTEST_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      RETURN
    END IF

  END FUNCTION CompTest_Release_IsValid


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CompTest_Is_Valid
!
! PURPOSE:
!       Elemental function to check the CompTest test type.
!
! CALLING SEQUENCE:
!       IsValid = CompTest_Is_Valid( CompTest )
!
! INPUTS:
!       CompTest:      CompTest object for which the test type component
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       TYPE(CompTest_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       IsValid:       Logical value defining the test type validity.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Scalar
!:sdoc-:
!----------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CompTest_Is_Valid(CompTest) RESULT(IsValid)
    TYPE(CompTest_type), INTENT(IN) :: CompTest
    LOGICAL :: IsValid
    IsValid = COUNT((/CompTest_IsFlagSet(CompTest,FWDTL_FLAG), &
                      CompTest_IsFlagSet(CompTest,TLAD_FLAG) , &
                      CompTest_IsFlagSet(CompTest,ADK_FLAG)   /)) == 1
  END FUNCTION CompTest_Is_Valid


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CompTest_Is_FWDTL
!
! PURPOSE:
!       Elemental helper function to indicate if the CompTest test type if FWD/TL.
!
! CALLING SEQUENCE:
!       result = CompTest_Is_FWDTL( CompTest )
!
! INPUTS:
!       CompTest:      CompTest object for which the test type component
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       TYPE(CompTest_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       result:        Logical value indicating if the test type is FWD/TL.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Scalar
!:sdoc-:
!----------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CompTest_Is_FWDTL(CompTest) RESULT(Is_FWDTL)
    TYPE(CompTest_type), INTENT(IN) :: CompTest
    LOGICAL :: Is_FWDTL
    Is_FWDTL = CompTest_IsFlagSet(CompTest,FWDTL_FLAG)
  END FUNCTION CompTest_Is_FWDTL


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CompTest_Is_TLAD
!
! PURPOSE:
!       Elemental helper function to indicate if the CompTest test type is TL/AD.
!
! CALLING SEQUENCE:
!       result = CompTest_Is_TLAD( CompTest )
!
! INPUTS:
!       CompTest:      CompTest object for which the test type component
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       TYPE(CompTest_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       result:        Logical value indicating if the test type is TL/AD.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Scalar
!:sdoc-:
!----------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CompTest_Is_TLAD(CompTest) RESULT(Is_TLAD)
    TYPE(CompTest_type), INTENT(IN) :: CompTest
    LOGICAL :: Is_TLAD
    Is_TLAD = CompTest_IsFlagSet(CompTest,TLAD_FLAG)
  END FUNCTION CompTest_Is_TLAD


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CompTest_Is_ADK
!
! PURPOSE:
!       Elemental helper function to indicate if the CompTest test type if AD/K.
!
! CALLING SEQUENCE:
!       result = CompTest_Is_ADK( CompTest )
!
! INPUTS:
!       CompTest:      CompTest object for which the test type component
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       TYPE(CompTest_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       result:        Logical value indicating if the test type is AD/K.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Scalar
!:sdoc-:
!----------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CompTest_Is_ADK(CompTest) RESULT(Is_ADK)
    TYPE(CompTest_type), INTENT(IN) :: CompTest
    LOGICAL :: Is_ADK
    Is_ADK = CompTest_IsFlagSet(CompTest,ADK_FLAG)
  END FUNCTION CompTest_Is_ADK


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CompTest_Set_FWDTL
!
! PURPOSE:
!       Elemental helper subroutine to set the CompTest test type to FWD/TL.
!
! CALLING SEQUENCE:
!       CALL CompTest_Set_FWDTL( CompTest )
!
! INPUT/OUTPUTS:
!       CompTest:      CompTest object for which the test type component
!                      is to be set.
!                      UNITS:      N/A
!                      TYPE:       TYPE(CompTest_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!:sdoc-:
!----------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CompTest_Set_FWDTL(CompTest)
    TYPE(CompTest_type), INTENT(IN OUT) :: CompTest
    CALL CompTest_SetFlag(CompTest,FWDTL_FLAG)
  END SUBROUTINE CompTest_Set_FWDTL


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CompTest_Set_TLAD
!
! PURPOSE:
!       Elemental helper subroutine to set the CompTest test type to TL/AD.
!
! CALLING SEQUENCE:
!       CALL CompTest_Set_TLAD( CompTest )
!
! INPUT/OUTPUTS:
!       CompTest:      CompTest object for which the test type component
!                      is to be set.
!                      UNITS:      N/A
!                      TYPE:       TYPE(CompTest_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!:sdoc-:
!----------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CompTest_Set_TLAD(CompTest)
    TYPE(CompTest_type), INTENT(IN OUT) :: CompTest
    CALL CompTest_SetFlag(CompTest,TLAD_FLAG)
  END SUBROUTINE CompTest_Set_TLAD


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CompTest_Set_ADK
!
! PURPOSE:
!       Elemental helper subroutine to set the CompTest test type to AD/K.
!
! CALLING SEQUENCE:
!       CALL CompTest_Set_ADK( CompTest )
!
! INPUT/OUTPUTS:
!       CompTest:      CompTest object for which the test type component
!                      is to be set.
!                      UNITS:      N/A
!                      TYPE:       TYPE(CompTest_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!:sdoc-:
!----------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CompTest_Set_ADK(CompTest)
    TYPE(CompTest_type), INTENT(IN OUT) :: CompTest
    CALL CompTest_SetFlag(CompTest,ADK_FLAG)
  END SUBROUTINE CompTest_Set_ADK


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CompTest_Is_Channel
!
! PURPOSE:
!       Elemental helper function to indicate if the CompTest spectral dimension
!       is based on sensor channels.
!
! CALLING SEQUENCE:
!       result = CompTest_Is_Channel( CompTest )
!
! INPUTS:
!       CompTest:      CompTest object for which the spectral dimension type
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       TYPE(CompTest_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       result:        Logical value indicating if the spectral dimension is
!                      based on sensor channels.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Scalar
!:sdoc-:
!----------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CompTest_Is_Channel(CompTest) RESULT(Is_Channel)
    TYPE(CompTest_type), INTENT(IN) :: CompTest
    LOGICAL :: Is_Channel
    Is_Channel = CompTest_IsFlagSet(CompTest,HAS_SPECTRAL_FLAG) .AND. &
                 (.NOT. CompTest_IsFlagSet(CompTest,FREQUENCY_FLAG))
  END FUNCTION CompTest_Is_Channel


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CompTest_Is_Frequency
!
! PURPOSE:
!       Elemental helper function to indicate if the CompTest spectral dimension
!       is based on frequency (i.e. high spectral resolution).
!
! CALLING SEQUENCE:
!       result = CompTest_Is_Frequency( CompTest )
!
! INPUTS:
!       CompTest:      CompTest object for which the spectral dimension type
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       TYPE(CompTest_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       result:        Logical value indicating if the spectral dimension is
!                      based on frequency.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Scalar
!:sdoc-:
!----------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CompTest_Is_Frequency(CompTest) RESULT(Is_Frequency)
    TYPE(CompTest_type), INTENT(IN) :: CompTest
    LOGICAL :: Is_Frequency
    Is_Frequency = CompTest_IsFlagSet(CompTest,HAS_SPECTRAL_FLAG) .AND. &
                   CompTest_IsFlagSet(CompTest,FREQUENCY_FLAG)
  END FUNCTION CompTest_Is_Frequency


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CompTest_Info
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about a CompTest object.
!
! CALLING SEQUENCE:
!       CALL CompTest_Info( CompTest, Info )
!
! OBJECTS:
!       CompTest:      CompTest object about which info is required.
!                      UNITS:      N/A
!                      TYPE:       TYPE(CompTest_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Info:          String containing version and dimension information
!                      about the passed CompTest object.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CompTest_Info( CTest, Info )
    ! Arguments
    TYPE(CompTest_type), INTENT(IN)  :: CTest
    CHARACTER(*),        INTENT(OUT) :: Info
    ! Parameters
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    ! Local variables
    CHARACTER(2000) :: Long_String

    ! Write the required data to the local string
    WRITE( Long_String, &
           '(a,1x,"CompTest RELEASE.VERSION: ",i2,".",i2.2,2x, &
           &"N_LAYERS=",i4,2x,&
           &a,"=",i4,2x,&
           &"N_PERTURBATIONS=",i4,2x,&
           &"N_INPUTVARS=",i4,2x,&
           &"N_OUTPUTVARS=",i4 )' ) &
           ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
           CTest%Release, CTest%Version, &
           CTest%nK, &
           CompTest_SpectralName(CTest), CTest%nL, &
           CTest%nP , &
           CTest%nIV, &
           CTest%nOV
                       
    ! Trim the output based on the
    ! dummy argument string length
    Info = Long_String(1:MIN(LEN(Info), LEN_TRIM(Long_String)))

  END SUBROUTINE CompTest_Info
  
  
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CompTest_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL CompTest_DefineVersion( Id )
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

  SUBROUTINE CompTest_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE CompTest_DefineVersion



  PURE FUNCTION CompTest_SpectralName( c ) RESULT( spectral_name )
    TYPE(CompTest_type), INTENT(IN) :: c
    CHARACTER(11) :: spectral_name
    IF ( CompTest_HasSpectral(c) ) THEN
      IF ( CompTest_Is_Frequency(c) ) THEN
        spectral_name = 'Frequencies'
      ELSE
        spectral_name = 'Channels   '
      END IF
    ELSE
      spectral_name = 'Spectral   '
    END IF
  END FUNCTION CompTest_SpectralName


  PURE FUNCTION CompTest_TestName( c ) RESULT( test_name )
    TYPE(CompTest_type), INTENT(IN) :: c
    CHARACTER(17) :: test_name
    IF ( .NOT. CompTest_Is_Valid(c) ) THEN
      test_name = 'Invalid!'
      RETURN
    END IF
    IF ( CompTest_Is_FWDTL(c) ) THEN
      test_name = 'FWD/TL model test'
    ELSE IF ( CompTest_Is_TLAD(c) ) THEN
      test_name = 'TL/AD model test '
    ELSE IF ( CompTest_Is_ADK(c) ) THEN
      test_name = 'AD/K model test  '
    END IF
  END FUNCTION CompTest_TestName


  PURE FUNCTION CompTest_d1Name( c ) RESULT( d1_name )
    TYPE(CompTest_type), INTENT(IN) :: c
    CHARACTER(5) :: d1_name
    IF ( .NOT. CompTest_Is_Valid(c) ) THEN
      d1_name = '*****'
      RETURN
    END IF
    IF ( CompTest_Is_FWDTL(c) ) THEN
      d1_name = 'd(NL)'
    ELSE IF ( CompTest_Is_TLAD(c) ) THEN
      d1_name = 'd(TL)'
    ELSE IF ( CompTest_Is_ADK(c) ) THEN
      d1_name = 'd(AD)'
    END IF
  END FUNCTION CompTest_d1Name

  PURE FUNCTION CompTest_d2Name( c ) RESULT( d2_name )
    TYPE(CompTest_type), INTENT(IN) :: c
    CHARACTER(5) :: d2_name
    IF ( .NOT. CompTest_Is_Valid(c) ) THEN
      d2_name = '*****'
      RETURN
    END IF
    IF ( CompTest_Is_FWDTL(c) ) THEN
      d2_name = 'd(TL)'
    ELSE IF ( CompTest_Is_TLAD(c) ) THEN
      d2_name = 'd(AD)'
    ELSE IF ( CompTest_Is_ADK(c) ) THEN
      d2_name = 'd(K)'
    END IF
  END FUNCTION CompTest_d2Name


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
!       CompTest_Equal
!
! PURPOSE:
!       Elemental function to test the equality of two CompTest objects.
!       Used in OPERATOR(==) interface block.
!
! CALLING SEQUENCE:
!       is_equal = CompTest_Equal( x, y )
!
!         or
!
!       IF ( x == y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:          Two CompTest objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       TYPE(CompTest_type)
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

  ELEMENTAL FUNCTION CompTest_Equal( x, y ) RESULT( is_equal )
    TYPE(CompTest_type), INTENT(IN)  :: x, y
    LOGICAL :: is_equal

    ! Set up
    is_equal = .FALSE.
    
    ! Check the object association status
    IF ( (.NOT. CompTest_Associated(x)) .OR. &
         (.NOT. CompTest_Associated(y))      ) RETURN

    ! Check contents
    ! ...Dimensions
    IF ( (x%nK  /= y%nK ) .OR. &
         (x%nL  /= y%nL ) .OR. &
         (x%nP  /= y%nP ) .OR. &
         (x%nIV /= y%nIV) .OR. &
         (x%nOV /= y%nOV) ) RETURN
    ! ...Scalars
    IF ( (x%nM      /= y%nM     ) .OR. &
         (x%nM_Name /= y%nM_Name) .OR. &
         (x%Flags   /= y%Flags  ) ) RETURN
    ! ...Data
    IF ( ALL(x%Pressure             .EqualTo. y%Pressure             ) .AND. &
         ALL(x%Spectral             .EqualTo. y%Spectral             ) .AND. &
         ALL(x%Perturbation         .EqualTo. y%Perturbation         ) .AND. &
         ALL(x%Input_Variable_Name     ==     y%Input_Variable_Name  ) .AND. &
         ALL(x%Input_Variable_Units    ==     y%Input_Variable_Units ) .AND. &
         ALL(x%Output_Variable_Name    ==     y%Output_Variable_Name ) .AND. &
         ALL(x%Output_Variable_Units   ==     y%Output_Variable_Units) .AND. &
         ALL(x%d1                   .EqualTo. y%d1                   ) .AND. &
         ALL(x%d2                   .EqualTo. y%d2                   )       ) &
      is_equal = .TRUE.

  END FUNCTION CompTest_Equal
  
END MODULE CompTest_Define
