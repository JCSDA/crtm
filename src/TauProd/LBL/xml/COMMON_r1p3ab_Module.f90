!
! COMMON_r1p3ab_Module
!
! Module containing procedures for COMMON Records 1.3a and 1.3b
!
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, 24-Dec-2012
!                     paul.vandelst@noaa.gov
!

MODULE COMMON_r1p3ab_Module

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
  PUBLIC :: COMMON_r1p3ab_type
  ! Procedures
  PUBLIC :: COMMON_r1p3ab_Associated
  PUBLIC :: COMMON_r1p3ab_Destroy
  PUBLIC :: COMMON_r1p3ab_Create
  PUBLIC :: COMMON_r1p3ab_SetValue
  PUBLIC :: COMMON_r1p3ab_Write


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! Message string length
  INTEGER, PARAMETER :: ML = 256
  ! The record I/O format
  CHARACTER(*), PARAMETER :: COMMON_R1P3A_FMT = '(39a1)'
  CHARACTER(*), PARAMETER :: COMMON_R1P3B_FMT = '(8es15.7)'


  ! -------------
  ! Derived types
  ! -------------
  TYPE :: COMMON_r1p3ab_type
    PRIVATE
    LOGICAL                   :: is_allocated = .FALSE.  ! Allocation indicator
    INTEGER                   :: n_absorbers  = 0        ! No. of calculation levels
    CHARACTER(1), ALLOCATABLE :: hmol_scal(:)            ! Profile scaling behaviour for selected species
    REAL(fp),     ALLOCATABLE :: xmol_scal(:)            ! Profile scaling factor for selected species
  END TYPE COMMON_r1p3ab_type


CONTAINS

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       COMMON_r1p3ab_Associated
!
! PURPOSE:
!       Pure function to test the status of the allocatable components
!       of the Common record 1.3a/1.3b object.
!
! CALLING SEQUENCE:
!       Status = COMMON_r1p3ab_Associated( record )
!
! OBJECTS:
!       record:    Common record 1.3a/1.3b structure which is to have its member's
!                  status tested.
!                  UNITS:      N/A
!                  TYPE:       COMMON_r1p3ab_type
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

  PURE FUNCTION COMMON_r1p3ab_Associated( self ) RESULT( status )
    TYPE(COMMON_r1p3ab_type), INTENT(IN) :: self
    LOGICAL :: status
    status = self%is_allocated
  END FUNCTION COMMON_r1p3ab_Associated

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       COMMON_r1p3ab_Destroy
!
! PURPOSE:
!       Pure subroutine to re-initialize Common record 1.3a/1.3b objects.
!
! CALLING SEQUENCE:
!       CALL COMMON_r1p3ab_Destroy( record )
!
! OBJECTS:
!       record:    Re-initialized  Common record 1.3a/1.3b structure.
!                  UNITS:      N/A
!                  TYPE:       COMMON_r1p3ab_type
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  PUREL SUBROUTINE COMMON_r1p3ab_Destroy( self )
    TYPE(COMMON_r1p3ab_type), INTENT(OUT) :: self
    self%Is_Allocated = .FALSE.
  END SUBROUTINE COMMON_r1p3ab_Destroy


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       COMMON_r1p3ab_Create
!
! PURPOSE:
!       Pure subroutine to create an instance of the Common record 1.3a/1.3b object.
!
! CALLING SEQUENCE:
!       CALL COMMON_r1p3ab_Create( &
!              record     , &
!              n_absorbers  )
!
! OBJECTS:
!       record:         Common record 1.3a/1.3b structure.
!                       UNITS:      N/A
!                       TYPE:       COMMON_r1p3ab_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_absorbers:    The number of gaseous absorbers.
!                       Must be > 0.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  PURE SUBROUTINE COMMON_r1p3ab_Create( &
    self       , &  ! Output
    n_absorbers  )  ! Input
    ! Arguments
    TYPE(COMMON_r1p3ab_type), INTENT(OUT) :: self
    INTEGER,                 INTENT(IN)  :: n_absorbers
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_absorbers < 1 ) RETURN

    ! Perform the allocation
    ALLOCATE( self%hmol_scal( n_absorbers ), &
              self%xmol_scal( n_absorbers ), &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN

    ! Initialise
    self%n_absorbers = n_absorbers
    self%hmol_scal   = ''
    self%xmol_scal   = ONE

    ! Set allocation indicator
    self%is_allocated = .TRUE.

  END SUBROUTINE COMMON_r1p3ab_Create


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       COMMON_r1p3ab_SetValue
!
! PURPOSE:
!       Subroutine to set the molecule amount scaling factor in the
!       Common record 1.3a/1.3b object.
!
! CALLING SEQUENCE:
!       CALL COMMON_r1p3ab_SetValue( record, scale_flag, scale_factor )
!
! OBJECTS:
!       record:        Common record 1.3a/1.3b structure which is to have its data set.
!                      - If unallocated, the object is allocated to the size of
!                        the input array.
!                      - If already allocated, the dimensions of the input array
!                        must be the same as that of the record allocated component
!                      UNITS:      N/A
!                      TYPE:       COMMON_r1p3ab_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!
! INPUTS:
!       scale_flag:    Array of character flags used to specify molecular profile
!                      scaling. The following describes the valid choices and
!                      their meaning:
!                        ' '       :  no scaling applied
!                        '0'       :  scaling factor is zero
!                        '1'       :  scaling factor used directly to scale profile
!                        'c' or 'C':  column amount to which the profile is to be
!                                     scaled (molec/cm^2)
!                        'd' or 'D':  column amount in Dobson units to which the
!                                     profile is to be scaled
!                        'm' or 'M':  volume mixing ratio (ppv) wrt dry air for the
!                                     total column to which the profile will be scaled
!                        'p' or 'P':  value of Precipitable Water Vapor (cm) to which
!                                     the profile will be scaled (water vapor only)
!
!                      The scaling factor is the same at all levels
!
!                      The molecule number, corresponding to the index in the scaling
!                      array, are as indicated in the following Table:
!
!                         1:   H2O  2:  CO2   3:    O3  4:   N2O  5:    CO
!                         6:   CH4  7:     O2 8:    NO  9:   SO2 10:   NO2
!                        11:   NH3 12:  HNO3 13:    OH 14:    HF 15:   HCL
!                        16:   HBR 17:    HI 18:   CLO 19:   OCS 20:  H2CO
!                        21:  HOCL 22:    N2 23:   HCN 24: CH3CL 25:  H2O2
!                        26:  C2H2 27:  C2H6 28:   PH3 29:  COF2 30:   SF6
!                        31:   H2S 32: HCOOH 33:   HO2 34:     O 35:CLONO2
!                        36:   NO+ 37:  HOBR 38:  C2H4 39: CH3OH
!
!                      UNITS:      N/A
!                      TYPE:       REAL(fp)
!                      DIMENSION:  Rank-1. Size must be > 0
!                      ATTRIBUTES: INTENT(IN)
!
!
!       scale_factor:  Factor to be used to scale the molecular profiles according
!                      to the interpretation of the scale_falg argument.
!                      UNITS:      Variable. See scale_falg argument.
!                      TYPE:       REAL(fp)
!                      DIMENSION:  Rank-1. Same size and scale_flag argument.
!                      ATTRIBUTES: INTENT(IN)
!
! COMMENTS:
!   If an error occurs, the record object is deallocated.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE COMMON_r1p3ab_SetValue( &
    self        , &  ! In/output
    scale_flag  , &  ! Input
    scale_factor  )  ! Input
    ! Arguments
    TYPE(COMMON_r1p3ab_1D_type), INTENT(IN OUT) :: self
    CHARACTER(*)               , INTENT(IN)     :: scale_flag(:)
    REAL(fp)                   , INTENT(IN)     :: scale_factor(:)
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'COMMON_r1p3ab_SetValue'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: n_absorbers

    ! Set up
    n_absorbers = SIZE(scale_flag)
    IF ( n_absorbers < 1 ) THEN
      msg = 'No absorber information specified'
      CALL Display_Message( ROUTINE_NAME, msg, FAILURE ); RETURN
    END IF
    IF ( SIZE(scale_factor) /= n_absorbers ) THEN
      msg = 'Input arrays are different sizes'
      CALL Display_Message( ROUTINE_NAME, msg, FAILURE ); RETURN
    END IF

    ! Allocate structure if necessary
    IF ( .NOT. COMMON_r1p3ab_Associated(self) ) THEN
      CALL COMMON_r1p3ab_Create( self, n_absorbers )
      IF ( .NOT. COMMON_r1p3ab_Associated(self) ) THEN
        msg = 'Allocation of COMMON_r1p3a structure failed'
        CALL Display_Message( ROUTINE_NAME, msg, FAILURE ); RETURN
      END IF
    END IF

    ! Check dimensions are consistent
    IF ( self%n_absorbers /= n_absorbers ) THEN
      WRITE( msg,'("Different size between structure (",i0,") and array (",i0,")")' ) &
                 self%n_absorbers, n_absorbers
      CALL Display_Message( ROUTINE_NAME, msg, FAILURE )
      CALL COMMON_r1p3ab_Destroy( self )
      RETURN
    END IF

    ! Assign the data
    self%scale_flag   = scale_flag
    self%scale_factor = scale_factor

  END SUBROUTINE COMMON_r1p3ab_SetValue


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       COMMON_r1p3ab_Write
!
! PURPOSE:
!       Function to write an instance of the Common record 1.3a/1.3b object.
!
! CALLING SEQUENCE:
!       error_status = COMMON_r1p3ab_Write( record, file_id )
!
! OBJECTS:
!       record:       Common record 1.3a/1.3b structure.
!                     UNITS:      N/A
!                     TYPE:       COMMON_r1p3ab_type
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
!       error_status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS, the write was successful
!                        == FAILURE, an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION COMMON_r1p3ab_Write(r1p3a,fid) RESULT(err_stat)
    ! Arguments
    TYPE(COMMON_r1p3ab_type), INTENT(IN) :: r1p3a
    INTEGER                 , INTENT(IN) :: fid
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'COMMON_r1p3ab_Write'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat

    ! Setup
    err_stat = SUCCESS
    ! ...Check there is something to write
    IF ( .NOT. COMMON_r1p3ab_Associated(self) ) THEN
      msg = 'Input data record is not allocated'
      CALL Cleanup(); RETURN
    END IF
    ! ...Check unit is open
    IF ( .NOT. File_Open(fid) ) THEN
      msg = 'File unit is not connected'
      CALL Cleanup(); RETURN
    END IF

    ! Write the records
    WRITE( fid,FMT=COMMON_R1P3A_FMT,IOSTAT=io_stat,IOMSG=io_msg) self%hmol_scal
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing record 1.3a - '//TRIM(io_msg)
      CALL Cleanup(); RETURN
    END IF
    WRITE( fid,FMT=COMMON_R1P3B_FMT,IOSTAT=io_stat,IOMSG=io_msg) self%xmol_scal
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing record 1.3b - '//TRIM(io_msg)
      CALL Cleanup(); RETURN
    END IF

  CONTAINS

    SUBROUTINE CleanUp()
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE CleanUp

  END FUNCTION COMMON_r1p3ab_Write

END MODULE COMMON_r1p3ab_Module
