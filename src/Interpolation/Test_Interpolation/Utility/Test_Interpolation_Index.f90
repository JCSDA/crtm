MODULE Test_Interpolation_Index

  ! Environment setup
  ! -----------------
  USE Type_Kinds
  USE Unit_Test
  USE CRTM_Interpolation

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: Test_Index_Regular
  PUBLIC :: Test_Index_Random
  
  ! Module parameters
  ! -----------------
  INTEGER, PARAMETER :: NIDX = 10
  INTEGER, PARAMETER :: N2 = NPTS/2


CONTAINS


  SUBROUTINE Test_Index_Regular(UTest)
    ! Arguments
    TYPE(UTest_type), INTENT(IN OUT) :: UTest
    ! Local variables
    REAL(fp) :: x, dx
    REAL(fp) :: x_regular(NIDX)
    INTEGER  :: i1, i2
    
    CALL init_test(UTest,'Regular index finding test',Caller='Test_Index_Regular')

    dx = 1.0_fp
    x_regular = (/ 0.5_fp, 1.5_fp, 2.5_fp, 3.5_fp, 4.5_fp, 5.5_fp, 6.5_fp, 7.5_fp, 8.5_fp, 9.5_fp /)
    
    ! In the middle
    x = 4.0_fp
    CALL find_index(x_regular,dx,x,i1,i2)
    CALL Assert((x>=x_regular(i1+N2-1)) .AND. (x<=x_regular(i2-N2+1)), UTest)
    ! Left edge
    x = 1.0_fp
    CALL find_index(x_regular,dx,x,i1,i2)
    CALL Is_Equal(1,   i1,UTest)
    CALL Is_Equal(NPTS,i2,UTest)
    ! Right edge
    x = 9.0_fp
    CALL find_index(x_regular,dx,x,i1,i2)
    CALL Is_Equal(NIDX,       i2,UTest)
    CALL Is_Equal(NIDX-NPTS+1,i1,UTest)
    ! Off the left edge
    x = -1.0_fp
    CALL find_index(x_regular,dx,x,i1,i2)
    CALL Is_Equal(1,   i1,UTest)
    CALL Is_Equal(NPTS,i2,UTest)
    ! Off the right edge
    x = 20.0_fp
    CALL find_index(x_regular,dx,x,i1,i2)
    CALL Is_Equal(NIDX,       i2,UTest)
    CALL Is_Equal(NIDX-NPTS+1,i1,UTest)

    CALL report_test(UTest)
    
  END SUBROUTINE Test_Index_Regular


  SUBROUTINE Test_Index_Random(UTest)
    ! Arguments
    TYPE(UTest_type), INTENT(IN OUT) :: UTest
    ! Local variables
    REAL(fp) :: x
    REAL(fp) :: x_random(NIDX)
    INTEGER  :: i1, i2
    
    CALL init_test(UTest,'Random index finding test',Caller='Test_Index_Random')

    x_random = (/ 0.5_fp, 1.0_fp, 1.013_fp, 2.0_fp, 4.0_fp, 7.9945_fp, 8.0_fp, 10.0_fp, 16.0_fp, 27.426_fp /)

    ! In the middle
    x = 5.0_fp
    CALL find_index(x_random,x,i1,i2)
    CALL Assert((x>=x_random(i1+N2-1)) .AND. (x<=x_random(i2-N2+1)), UTest)
    ! Left edge
    x = 0.75_fp
    CALL find_index(x_random,x,i1,i2)
    CALL Is_Equal(1,   i1,UTest)
    CALL Is_Equal(NPTS,i2,UTest)
    ! Right edge
    x = 17.0_fp
    CALL find_index(x_random,x,i1,i2)
    CALL Is_Equal(NIDX,       i2,UTest)
    CALL Is_Equal(NIDX-NPTS+1,i1,UTest)
    ! Off the left edge
    x = -1.0_fp
    CALL find_index(x_random,x,i1,i2)
    CALL Is_Equal(1,   i1,UTest)
    CALL Is_Equal(NPTS,i2,UTest)
    ! Off the right edge
    x = 28.0_fp
    CALL find_index(x_random,x,i1,i2)
    CALL Is_Equal(NIDX,       i2,UTest)
    CALL Is_Equal(NIDX-NPTS+1,i1,UTest)

    CALL report_test(UTest)
      
  END SUBROUTINE Test_Index_Random

END MODULE Test_Interpolation_Index
