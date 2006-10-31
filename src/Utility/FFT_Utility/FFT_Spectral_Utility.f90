!
! FFT_Spectral_Utility
!
MODULE FFT_Spectral_Utility
  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds,            ONLY: fp=>fp_kind
  USE Message_Handler,       ONLY: SUCCESS, FAILURE, Display_Message
  USE Fundamental_Constants, ONLY: PI
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Visibility
  ! ----------
  PRIVATE
  PUBLIC :: CosFilter

  
CONTAINS


  FUNCTION CosFilter(Frequency   , & ! Input
                     Filter      , & ! Output
                     RolloffWidth, & ! Optional Input
                     Reverse     , & ! Optional Input
                     Message_Log ) & ! Optional Input
                    RESULT(Error_Status)
    ! Arguments
    REAL(fp), DIMENSION(:), INTENT(IN)  :: Frequency
    REAL(fp), DIMENSION(:), INTENT(OUT) :: Filter
    REAL(fp),     OPTIONAL, INTENT(IN)  :: RolloffWidth
    INTEGER,      OPTIONAL, INTENT(IN)  :: Reverse
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Locall parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CosFilter'
    REAL(fp), PARAMETER :: POINT5        = 0.5_fp
    REAL(fp), PARAMETER :: ONE           = 1.0_fp
    REAL(fp), PARAMETER :: ONEpointFIVE  = 1.5_fp
    REAL(fp), PARAMETER :: DEFAULT_WIDTH = 10.0_fp
    ! Local variables
    REAL(fp) :: Width
    INTEGER  :: n, nFilterPts
    INTEGER  :: i1, i2, i3
    REAL(fp) :: dF

    Error_Status = SUCCESS
    
    Width = DEFAULT_WIDTH
    IF (PRESENT(RolloffWidth)) Width=RolloffWidth

    n = SIZE(Frequency)
    ! Mean frequency interval
    dF = SUM( (Frequency(2:n)-Frequency(1:n-1)) ) / REAL(n-1,fp)
    ! How many points required for filter
    nFilterPts = INT( ( Width / dF ) + ONEpointFIVE )
    IF(nFilterPts <= 1)THEN
      Error_Status = FAILURE
      CALL Display_Message(ROUTINE_NAME, &
                           'Number of filter points too small', &
                           Error_Status, &
                           Message_Log=Message_Log )
      RETURN
    END IF

    i1 = nFilterPts
    i2 = 1
    i3 = -1
    IF (PRESENT(Reverse)) THEN
      IF (Reverse==1) THEN
        i1 = n-nFilterPts+1
        i2 = n
        i3 = 1
      END IF
    END IF

    Filter(i1:i2:i3) = POINT5 * (ONE + COS((Frequency(i1:i2:i3)-Frequency(i1))*PI/Width))
    
  END FUNCTION CosFilter 

END MODULE FFT_Spectral_Utility
