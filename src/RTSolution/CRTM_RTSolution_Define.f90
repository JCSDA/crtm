!
! CRTM_RTSolution_Define
!
! Module defining the CRTM RTSolution structure and containing routines
! to manipulate it.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 13-May-2004
!                       paul.vandelst@noaa.gov
!

MODULE CRTM_RTSolution_Define


  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use statements
  USE Type_Kinds           , ONLY: fp
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE Compare_Float_Numbers, ONLY: DEFAULT_N_SIGFIG, &
                                   OPERATOR(.EqualTo.), &
                                   Compares_Within_Tolerance
  USE CRTM_Parameters      , ONLY: ZERO, STRLEN
  USE SensorInfo_Parameters, ONLY: INVALID_SENSOR, &
                                   INVALID_WMO_SATELLITE_ID, &
                                   INVALID_WMO_SENSOR_ID
  ! Disable all implicit typing
  IMPLICIT NONE


  ! --------------------
  ! Default visibilities
  ! --------------------
  ! Everything private by default
  PRIVATE
  ! Datatypes
  PUBLIC :: CRTM_RTSolution_type
  ! Operators
  PUBLIC :: OPERATOR(==)
  ! Public procedures
  PUBLIC :: CRTM_RTSolution_Associated
  PUBLIC :: CRTM_RTSolution_Destroy
  PUBLIC :: CRTM_RTSolution_Create
  PUBLIC :: CRTM_RTSolution_Zero
  PUBLIC :: CRTM_RTSolution_Inspect
  PUBLIC :: CRTM_RTSolution_DefineVersion
  PUBLIC :: CRTM_RTSolution_Compare

  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE CRTM_RTSolution_Equal
  END INTERFACE OPERATOR(==)

  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id$'
  ! Message string length
  INTEGER, PARAMETER :: ML = 256

  ! -------------------------------
  ! RTSolution data type definition
  ! -------------------------------
  !:tdoc+:
  TYPE :: CRTM_RTSolution_type
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! Dimensions
    INTEGER :: n_Layers = 0  ! K
    ! Sensor information
    CHARACTER(STRLEN) :: Sensor_ID        = ''
    INTEGER           :: WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID
    INTEGER           :: WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID
    INTEGER           :: Sensor_Channel   = 0    
    ! RT algorithm information
    CHARACTER(STRLEN) :: RT_Algorithm_Name = ''
    ! Internal variables. Users do not need to worry about these.
    LOGICAL :: Scattering_Flag = .TRUE.
    INTEGER :: n_Full_Streams  = 0
    INTEGER :: n_Stokes        = 0
    ! Forward radiative transfer intermediate results for a single channel
    !    These components are not defined when they are used as TL, AD
    !    and K variables
    REAL(fp) :: SOD                     = ZERO  ! Scattering Optical Depth
    REAL(fp) :: Surface_Emissivity      = ZERO
    REAL(fp) :: Up_Radiance             = ZERO
    REAL(fp) :: Down_Radiance           = ZERO
    REAL(fp) :: Down_Solar_Radiance     = ZERO
    REAL(fp) :: Surface_Planck_Radiance = ZERO
    REAL(fp), ALLOCATABLE :: Upwelling_Radiance(:)   ! K
    REAL(fp), ALLOCATABLE :: Layer_Optical_Depth(:)  ! K
    ! Radiative transfer results for a single channel/node
    REAL(fp) :: Radiance               = ZERO
    REAL(fp) :: Brightness_Temperature = ZERO
  END TYPE CRTM_RTSolution_type
  !:tdoc-:


CONTAINS


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                           ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_RTSolution_Associated
!
! PURPOSE:
!       Elemental function to test the status of the allocatable components
!       of a CRTM RTSolution object.
!
! CALLING SEQUENCE:
!       Status = CRTM_RTSolution_Associated( RTSolution )
!
! OBJECTS:
!       RTSolution:   RTSolution structure which is to have its member's
!                     status tested.
!                     UNITS:      N/A
!                     TYPE:       CRTM_RTSolution_type
!                     DIMENSION:  Scalar or any rank
!                     ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:       The return value is a logical value indicating the
!                     status of the RTSolution members.
!                       .TRUE.  - if the array components are allocated.
!                       .FALSE. - if the array components are not allocated.
!                     UNITS:      N/A
!                     TYPE:       LOGICAL
!                     DIMENSION:  Same as input RTSolution argument
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_RTSolution_Associated( RTSolution ) RESULT( Status )
    TYPE(CRTM_RTSolution_type), INTENT(IN) :: RTSolution
    LOGICAL :: Status
    Status = RTSolution%Is_Allocated
  END FUNCTION CRTM_RTSolution_Associated


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_RTSolution_Destroy
! 
! PURPOSE:
!       Elemental subroutine to re-initialize CRTM RTSolution objects.
!
! CALLING SEQUENCE:
!       CALL CRTM_RTSolution_Destroy( RTSolution )
!
! OBJECTS:
!       RTSolution:   Re-initialized RTSolution structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_RTSolution_type
!                     DIMENSION:  Scalar OR any rank
!                     ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_RTSolution_Destroy( RTSolution )
    TYPE(CRTM_RTSolution_type), INTENT(OUT) :: RTSolution
    RTSolution%Is_Allocated = .FALSE.
    RTSolution%n_Layers = 0
  END SUBROUTINE CRTM_RTSolution_Destroy
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_RTSolution_Create
! 
! PURPOSE:
!       Elemental subroutine to create an instance of the CRTM RTSolution object.
!
! CALLING SEQUENCE:
!       CALL CRTM_RTSolution_Create( RTSolution, n_Layers )
!
! OBJECTS:
!       RTSolution:   RTSolution structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_RTSolution_type
!                     DIMENSION:  Scalar or any rank
!                     ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_Layers:     Number of layers for which there is RTSolution data.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Same as RTSolution object
!                     ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_RTSolution_Create( RTSolution, n_Layers )
    ! Arguments
    TYPE(CRTM_RTSolution_type), INTENT(OUT) :: RTSolution
    INTEGER,                    INTENT(IN)  :: n_Layers
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_Layers < 1 ) RETURN

    ! Perform the allocation
    ALLOCATE( RTSolution%Upwelling_Radiance(n_Layers), &
              RTSolution%Layer_Optical_Depth(n_Layers), &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN

    ! Initialise
    ! ...Dimensions
    RTSolution%n_Layers = n_Layers
    ! ...Arrays
    RTSolution%Upwelling_Radiance  = ZERO
    RTSolution%Layer_Optical_Depth = ZERO
    
    ! Set allocation indicator
    RTSolution%Is_Allocated = .TRUE.

  END SUBROUTINE CRTM_RTSolution_Create


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_RTSolution_Zero
! 
! PURPOSE:
!       Elemental subroutine to zero out the data components
!       in a CRTM RTSolution object.
!
! CALLING SEQUENCE:
!       CALL CRTM_RTSolution_Zero( rts )
!
! OUTPUTS:
!       rts:          CRTM RTSolution structure in which the data components
!                     are to be zeroed out.
!                     UNITS:      N/A
!                     TYPE:       CRTM_RTSolution_type
!                     DIMENSION:  Scalar or any rank
!                     ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       - The dimension components of the structure are *NOT* set to zero.
!       - The sensor infomration and RT algorithm components are
!         *NOT* reset in this routine.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_RTSolution_Zero( RTSolution )
    TYPE(CRTM_RTSolution_type), INTENT(IN OUT) :: RTSolution

    ! Zero out the scalar data components
    RTSolution%SOD                     = ZERO
    RTSolution%Surface_Emissivity      = ZERO
    RTSolution%Up_Radiance             = ZERO
    RTSolution%Down_Radiance           = ZERO
    RTSolution%Down_Solar_Radiance     = ZERO
    RTSolution%Surface_Planck_Radiance = ZERO
    RTSolution%Radiance                = ZERO
    RTSolution%Brightness_Temperature  = ZERO

    ! Zero out the array data components
    IF ( CRTM_RTSolution_Associated(RTSolution) ) THEN
      RTSolution%Upwelling_Radiance  = ZERO
      RTSolution%Layer_Optical_Depth = ZERO
    END IF

  END SUBROUTINE CRTM_RTSolution_Zero


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_RTSolution_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of a CRTM RTSolution object to stdout.
!
! CALLING SEQUENCE:
!       CALL CRTM_RTSolution_Inspect( RTSolution )
!
! INPUTS:
!       RTSolution:    CRTM RTSolution object to display.
!                      UNITS:      N/A
!                      TYPE:       CRTM_RTSolution_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_RTSolution_Inspect( RTSolution )
    TYPE(CRTM_RTSolution_type), INTENT(IN) :: RTSolution
    WRITE(*,'(1x,"RTSolution OBJECT")')
    ! Display components
    WRITE(*,'(3x,"Sensor Id               : ",a )') TRIM(RTSolution%Sensor_ID)
    WRITE(*,'(3x,"Channel                 : ",i0)') RTSolution%Sensor_Channel
    WRITE(*,'(3x,"RT Algorithm Name       : ",a )') RTSolution%RT_Algorithm_Name
    WRITE(*,'(3x,"Surface Emissivity      : ",es13.6)') RTSolution%Surface_Emissivity          
    WRITE(*,'(3x,"Up Radiance             : ",es13.6)') RTSolution%Up_Radiance                 
    WRITE(*,'(3x,"Down Radiance           : ",es13.6)') RTSolution%Down_Radiance               
    WRITE(*,'(3x,"Down Solar Radiance     : ",es13.6)') RTSolution%Down_Solar_Radiance         
    WRITE(*,'(3x,"Surface Planck Radiance : ",es13.6)') RTSolution%Surface_Planck_Radiance     
    IF ( CRTM_RTSolution_Associated(RTSolution) ) THEN
      WRITE(*,'(3x,"n_Layers : ",i0)') RTSolution%n_Layers
      WRITE(*,'(3x,"Upwelling Radiance      :")') 
      WRITE(*,'(5(1x,es13.6,:))') RTSolution%Upwelling_Radiance
      WRITE(*,'(3x,"Layer Optical Depth     :")') 
      WRITE(*,'(5(1x,es13.6,:))') RTSolution%Layer_Optical_Depth
    END IF
    WRITE(*,'(3x,"Radiance                : ",es13.6)') RTSolution%Radiance                   
    WRITE(*,'(3x,"Brightness Temperature  : ",es13.6)') RTSolution%Brightness_Temperature     
    
  END SUBROUTINE CRTM_RTSolution_Inspect

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_RTSolution_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL CRTM_RTSolution_DefineVersion( Id )
!
! OUTPUTS:
!       Id:            Character string containing the version Id information
!                      for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_RTSolution_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE CRTM_RTSolution_DefineVersion
     
!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       CRTM_RTSolution_Compare
!
! PURPOSE:
!       Elemental function to compare two CRTM_RTSolution objects to within
!       a user specified number of significant figures.
!
! CALLING SEQUENCE:
!       is_comparable = CRTM_RTSolution_Compare( x, y, n_SigFig=n_SigFig )
!
! OBJECTS:
!       x, y:          Two CRTM RTSolution objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       CRTM_RTSolution_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       n_SigFig:      Number of significant figure to compare floating point
!                      components.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Conformable with inputs
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       is_comparable: Logical value indicating whether the inputs are
!                      comparable.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Same as inputs.
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_RTSolution_Compare( &
    x, &
    y, &
    n_SigFig ) &
  RESULT( is_comparable )
    TYPE(CRTM_RTSolution_type), INTENT(IN) :: x, y
    INTEGER,          OPTIONAL, INTENT(IN) :: n_SigFig
    LOGICAL :: is_comparable
    ! Variables
    INTEGER :: n

    ! Set up
    is_comparable = .FALSE.
    IF ( PRESENT(n_SigFig) ) THEN
      n = ABS(n_SigFig)
    ELSE
      n = DEFAULT_N_SIGFIG
    END IF
    
    ! Check the structure association status
    IF ( .NOT. (CRTM_RTSolution_Associated(x) .EQV. CRTM_RTSolution_Associated(y)) ) RETURN

    ! Check the sensor informatin
    IF ( (x%Sensor_ID        /= y%Sensor_ID       ) .OR. &
         (x%WMO_Satellite_ID /= y%WMO_Satellite_ID) .OR. &
         (x%WMO_Sensor_ID    /= y%WMO_Sensor_ID   ) .OR. &
         (x%Sensor_Channel   /= y%Sensor_Channel  ) ) RETURN 
         
    ! Check the RT algorithm name   
    IF ( x%RT_Algorithm_Name /= y%RT_Algorithm_Name ) RETURN
    
    ! Check the scalar components
    IF ( .NOT. Compares_Within_Tolerance(x%Surface_Emissivity     , y%Surface_Emissivity     , n) .OR. &
         .NOT. Compares_Within_Tolerance(x%Up_Radiance            , y%Up_Radiance            , n) .OR. &
         .NOT. Compares_Within_Tolerance(x%Down_Radiance          , y%Down_Radiance          , n) .OR. &
         .NOT. Compares_Within_Tolerance(x%Down_Solar_Radiance    , y%Down_Solar_Radiance    , n) .OR. &
         .NOT. Compares_Within_Tolerance(x%Surface_Planck_Radiance, y%Surface_Planck_Radiance, n) .OR. &
         .NOT. Compares_Within_Tolerance(x%Radiance               , y%Radiance               , n) .OR. &
         .NOT. Compares_Within_Tolerance(x%Brightness_Temperature , y%Brightness_Temperature , n) ) RETURN

    ! Check the array components
    IF ( CRTM_RTSolution_Associated(x) .AND. CRTM_RTSolution_Associated(y) ) THEN
      IF ( (.NOT. ALL(Compares_Within_Tolerance(x%Upwelling_Radiance ,y%Upwelling_Radiance ,n))) .OR. &
           (.NOT. ALL(Compares_Within_Tolerance(x%Layer_Optical_Depth,y%Layer_Optical_Depth,n))) ) RETURN
    END IF
        
    ! If we get here, the structures are comparable
    is_comparable = .TRUE.
    
  END FUNCTION CRTM_RTSolution_Compare


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
!       CRTM_RTSolution_Equal
!
! PURPOSE:
!       Elemental function to test the equality of two CRTM_RTSolution objects.
!       Used in OPERATOR(==) interface block.
!
! CALLING SEQUENCE:
!       is_equal = CRTM_RTSolution_Equal( x, y )
!
!         or
!
!       IF ( x == y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:          Two CRTM RTSolution objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       CRTM_RTSolution_type
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

  ELEMENTAL FUNCTION CRTM_RTSolution_Equal( x, y ) RESULT( is_equal )
    TYPE(CRTM_RTSolution_type) , INTENT(IN)  :: x, y
    LOGICAL :: is_equal

    ! Setup
    is_equal = .FALSE.
    
    ! Check the structure association status
    IF ( .NOT. (CRTM_RTSolution_Associated(x) .EQV. CRTM_RTSolution_Associated(y)) ) RETURN
    
    ! Check scalars
    IF ( (x%n_Layers == y%n_Layers) .AND. &
         (x%Sensor_ID         == y%Sensor_ID        ) .AND. &
         (x%WMO_Satellite_ID  == y%WMO_Satellite_ID ) .AND. &
         (x%WMO_Sensor_ID     == y%WMO_Sensor_ID    ) .AND. &
         (x%Sensor_Channel    == y%Sensor_Channel   ) .AND. &
         (x%RT_Algorithm_Name == y%RT_Algorithm_Name) .AND. &
         (x%Surface_Emissivity      .EqualTo. y%Surface_Emissivity     ) .AND. &
         (x%Up_Radiance             .EqualTo. y%Up_Radiance            ) .AND. &
         (x%Down_Radiance           .EqualTo. y%Down_Radiance          ) .AND. &
         (x%Down_Solar_Radiance     .EqualTo. y%Down_Solar_Radiance    ) .AND. &
         (x%Surface_Planck_Radiance .EqualTo. y%Surface_Planck_Radiance) .AND. &
         (x%Radiance                .EqualTo. y%Radiance               ) .AND. &
         (x%Brightness_Temperature  .EqualTo. y%Brightness_Temperature ) ) &
      is_equal = .TRUE.

    ! Check arrays (which may or may not be allocated)
    IF ( CRTM_RTSolution_Associated(x) .AND. CRTM_RTSolution_Associated(y) ) THEN
      is_equal = is_equal .AND. &
                 ALL(x%Upwelling_Radiance  .EqualTo. y%Upwelling_Radiance ) .AND. &
                 ALL(x%Layer_Optical_Depth .EqualTo. y%Layer_Optical_Depth)
    END IF
    
  END FUNCTION CRTM_RTSolution_Equal
  
END MODULE CRTM_RTSolution_Define
