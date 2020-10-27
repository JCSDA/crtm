!
! COMMON_r3p3b_Module
!
! Module containing procedures for COMMON Record r3p3b.
!
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, 24-Dec-2012
!                     paul.vandelst@noaa.gov
!

MODULE COMMON_r3p3b_Module

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds     , ONLY: fp
  USE File_Utility   , ONLY: File_Open
  USE Message_Handler, ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  ! Line-by-line model parameters
  USE LBL_Parameters
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Visibility
  ! ----------
  ! Everything private by default
  PRIVATE
  ! Datatypes
  PUBLIC :: COMMON_r3p3b_type
  ! Procedures
  PUBLIC :: COMMON_r3p3b_Associated
  PUBLIC :: COMMON_r3p3b_Destroy
  PUBLIC :: COMMON_r3p3b_Create
  PUBLIC :: COMMON_r3p3b_SetValue
  PUBLIC :: COMMON_r3p3b_Write


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! Message string length
  INTEGER, PARAMETER :: ML = 256
  ! The record I/O format
  CHARACTER(*), PARAMETER :: COMMON_R3P3B_FMT = '(8f10.3)'


  ! -------------
  ! Derived types
  ! -------------
  TYPE :: COMMON_r3p3b_type
    PRIVATE
    LOGICAL               :: is_allocated         = .FALSE.  ! Allocation indicator
    LOGICAL               :: is_pressure          = .FALSE.  ! Data type flag
    INTEGER               :: n_calculation_levels = 0        ! No. of calculation levels
    REAL(fp), ALLOCATABLE :: bnd(:)                          ! Altitudes/pressures of layer boundaries
  END TYPE COMMON_r3p3b_type


CONTAINS

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       COMMON_r3p3b_Associated
!
! PURPOSE:
!       Pure function to test the status of the allocatable components
!       of an Common record 3.3b object.
!
! CALLING SEQUENCE:
!       Status = COMMON_r3p3b_Associated( record )
!
! OBJECTS:
!       record:    Common record 3.3b structure which is to have its member's
!                  status tested.
!                  UNITS:      N/A
!                  TYPE:       COMMON_r3p3b_type
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:    The return value is a logical value indicating the
!                  status of the object members.
!                    .TRUE.  - if the array components are allocated.
!                    .FALSE. - if the array components are not allocated.
!                  UNITS:      N/A
!                  TYPE:       LOGICAL
!                  DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  PURE FUNCTION COMMON_r3p3b_Associated( self ) RESULT( Status )
    TYPE(COMMON_r3p3b_type), INTENT(IN) :: self
    LOGICAL :: Status
    Status = self%is_allocated
  END FUNCTION COMMON_r3p3b_Associated

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       COMMON_r3p3b_Destroy
!
! PURPOSE:
!       Pure subroutine to re-initialize Common record 3.3b objects.
!
! CALLING SEQUENCE:
!       CALL COMMON_r3p3b_Destroy( record )
!
! OBJECTS:
!       record:    Re-initialized COMMON_r3p3b structure.
!                  UNITS:      N/A
!                  TYPE:       COMMON_r3p3b_type
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  PUREL SUBROUTINE COMMON_r3p3b_Destroy( self )
    TYPE(COMMON_r3p3b_type), INTENT(OUT) :: self
    self%Is_Allocated = .FALSE.
  END SUBROUTINE COMMON_r3p3b_Destroy


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       COMMON_r3p3b_Create
!
! PURPOSE:
!       Pure subroutine to create an instance of the Common record 3.3b object.
!
! CALLING SEQUENCE:
!       CALL COMMON_r3p3b_Create( &
!              record                   , &
!              n_calculation_levels     , &
!              is_pressure = is_pressure  )
!
! OBJECTS:
!       record:                 Common record 3.3b structure.
!                               UNITS:      N/A
!                               TYPE:       COMMON_r3p3b_type
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_calculation_levels:   The number of LBLRTM calculation LEVELS, i.e. the
!                               number of calculation level boundaries.
!                               If not specified, a default layering configuration
!                               is used.
!                               Must be > 1.
!                               UNITS:      N/A
!                               TYPE:       INTEGER
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       is_pressure:            Logical flag to indicate the boundary values are
!                               specified as pressure levels.
!                               If == .FALSE., boundaries are specified as altitudes [DEFAULT]
!                                  == .TRUE.,  boundaries are specified as pressures.
!                               UNITS:      N/A
!                               TYPE:       LOGICAL
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT(IN), OPTIONAL
!
!:sdoc-:
!--------------------------------------------------------------------------------

  PURE SUBROUTINE COMMON_r3p3b_Create( &
    self                , &  ! Output
    n_calculation_levels, &  ! Input
    is_pressure           )  ! Optional input
    ! Arguments
    TYPE(COMMON_r3p3b_type), INTENT(OUT) :: self
    INTEGER,                 INTENT(IN)  :: n_calculation_levels
    LOGICAL,       OPTIONAL, INTENT(IN)  :: is_pressure
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_calculation_levels < 2 ) RETURN

    ! Perform the allocation
    ALLOCATE( self%bnd( n_calculation_levels ), &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN

    ! Initialise
    IF ( PRESENT(is_pressure) ) self%is_pressure = is_pressure
    self%n_calculation_levels = n_calculation_levels
    self%bnd                  = ZERO

    ! Set allocation indicator
    self%is_allocated = .TRUE.

  END SUBROUTINE COMMON_r3p3b_Create


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       COMMON_r3p3b_SetValue
!
! PURPOSE:
!       Subroutine to set the calculation layer boundaries in the
!       Common record 3.3b object.
!
! CALLING SEQUENCE:
!       CALL COMMON_r3p3b_SetValue( record, bnd )
!
! OBJECTS:
!       record:     Common record 3.3b structure which is to have its data set.
!                   - If unallocated, the object is allocated to the size of
!                     the input array.
!                   - If already allocated, the dimensions of the input array
!                     must be the same as that of the record allocated component
!                   UNITS:      N/A
!                   TYPE:       COMMON_r3p3b_type
!                   DIMENSION:  Scalar
!                   ATTRIBUTES: INTENT(IN OUT)
!
! INPUTS:
!       bnd:        Array of calculation level boundary altitudes or pressures,
!                   ordered from surface to top-of-atmosphere.
!                   UNITS:      km, or hPa
!                   TYPE:       REAL(fp)
!                   DIMENSION:  Rank-1. Size must be > 1
!                   ATTRIBUTES: INTENT(IN)
!
! COMMENTS:
!   If an error occurs, the record object is deallocated.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE COMMON_r3p3b_SetValue( &
    self, &  ! In/output
    bnd   )  ! Input
    ! Arguments
    TYPE(COMMON_r3p3b_1D_type), INTENT(IN OUT) :: self
    REAL(fp)                  , INTENT(IN)     :: bnd(:)
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'COMMON_r3p3b_SetValue'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: n_calculation_levels

    ! Set up
    n_calculation_levels = SIZE(bnd)

    ! Allocate structure if necessary
    IF ( .NOT. COMMON_r3p3b_Associated(self) ) THEN
      IF ( n_calculation_levels < 2 ) THEN
        msg = 'Must specify at least two level boundaries'
        CALL Display_Message( ROUTINE_NAME, msg, FAILURE ); RETURN
      END IF
      CALL COMMON_r3p3b_Create( self, n_calculation_levels, is_pressure = bnd(2) < bnd(1) )
      IF ( .NOT. COMMON_r3p3b_Associated(self) ) THEN
        msg = 'Allocation of COMMON_r3p3b structure failed'
        CALL Display_Message( ROUTINE_NAME, msg, FAILURE ); RETURN
      END IF
    END IF

    ! Check dimensions are consistent
    IF ( self%n_calculation_levels /= n_calculation_levels ) THEN
      WRITE( msg,'("Different size between structure (",i0,") and array (",i0,")")' ) &
                 self%n_calculation_levels, n_calculation_levels
      CALL Display_Message( ROUTINE_NAME, msg, FAILURE )
      CALL COMMON_r3p3b_Destroy( self )
      RETURN
    END IF

    ! Assign the boundary level data
    self%bnd = bnd

  END SUBROUTINE COMMON_r3p3b_SetValue


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       COMMON_r3p3b_Write
!
! PURPOSE:
!       Function to write an instance of the Common record 3.3b object.
!
! CALLING SEQUENCE:
!       error_status = COMMON_r3p3b_Write( record, file_id )
!
! OBJECTS:
!       record:       Common record 3.3b structure.
!                     UNITS:      N/A
!                     TYPE:       COMMON_r3p3b_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! INPUTS:
!       file_id:      The unit number of the already opened file to which the
!                     record is to be written.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS, the write was successful
!                        == FAILURE, an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION COMMON_r3p3b_Write(self,fid) RESULT(err_stat)
    ! Arguments
    TYPE(COMMON_r3p3b_type), INTENT(IN) :: self
    INTEGER                , INTENT(IN) :: fid
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'COMMON_r3p3b_Write'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat

    ! Setup
    err_stat = SUCCESS
    ! ...Check there is something to write
    IF ( .NOT. COMMON_r3p3b_Associated(self) ) THEN
      msg = 'Input data record is not allocated'
      CALL Cleanup(); RETURN
    END IF
    ! ...Check unit is open
    IF ( .NOT. File_Open(fid) ) THEN
      msg = 'File unit is not connected'
      CALL Cleanup(); RETURN
    END IF

    ! Write the record
    WRITE( fid,FMT=COMMON_R3P3B_FMT,IOSTAT=io_stat,IOMSG=io_msg) self%bnd
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing record - '//TRIM(io_msg)
      CALL Cleanup(); RETURN
    END IF

  CONTAINS

    SUBROUTINE CleanUp()
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE CleanUp

  END FUNCTION COMMON_r3p3b_Write

END MODULE COMMON_r3p3b_Module
