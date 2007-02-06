MODULE Unit_Test
  USE Type_Kinds, ONLY: fp
  USE Compare_Float_Numbers
  IMPLICIT NONE
  
  PRIVATE
  PUBLIC :: assert_equal
  PUBLIC :: assert_notequal
  PUBLIC :: init_alltests
  PUBLIC :: init_test
  PUBLIC :: report_test
  PUBLIC :: report_alltests

  INTERFACE assert_equal
    MODULE PROCEDURE ip_equal
    MODULE PROCEDURE fp_equal_scalar
    MODULE PROCEDURE fp_equal_rank1
  END INTERFACE assert_equal
  
  INTERFACE assert_notequal
    MODULE PROCEDURE ip_notequal
    MODULE PROCEDURE fp_notequal_scalar
    MODULE PROCEDURE fp_notequal_rank1
  END INTERFACE assert_notequal
  
  INTEGER, SAVE :: n_tests     , n_passed_tests     , n_failed_tests
  INTEGER, SAVE :: n_alltests=0, n_passed_alltests=0, n_failed_alltests=0
  LOGICAL, SAVE :: noisy = .FALSE.

CONTAINS

  ! ------
  ! Public
  ! ------
  SUBROUTINE ip_equal(i1,i2)
    INTEGER, INTENT(IN) :: i1,i2
    IF ( i1 == i2 ) THEN
      IF (noisy) THEN
        WRITE(*,'(4x,"Assert equal passed. Expected ", i0, " and got ", i0 )') i1, i2
      END IF
      CALL test_passed()
    ELSE
      WRITE(*,'(4x,"Assert equal FAILED. Expected ", i0, " but got ", i0 )') i1, i2
      CALL test_failed()
    END IF
  END SUBROUTINE ip_equal
  
  SUBROUTINE fp_equal_scalar(r1,r2, ulp)
    REAL(fp),          INTENT(IN) :: r1,r2
    INTEGER, OPTIONAL, INTENT(IN) :: ulp
    CHARACTER(*), PARAMETER :: RFMT = 'es25.18'
    INTEGER :: rel
    rel = 1
    IF ( PRESENT(ulp) ) rel = ulp
    IF ( Compare_Float(r1,r2,ULP=rel) ) THEN
      IF (noisy) THEN
        WRITE(*,'(4x,"Assert equal passed. Expected ", '//RFMT//', " and got ", '//RFMT//' )') r1,r2
      END IF
      CALL test_passed()
    ELSE
      WRITE(*,'(4x,"Assert equal FAILED. Expected ", '//RFMT//', " but got ", '//RFMT//' )') r1,r2
      CALL test_failed()
    END IF
  END SUBROUTINE fp_equal_scalar

  SUBROUTINE fp_equal_rank1(r1,r2, ulp)
    REAL(fp),          INTENT(IN) :: r1(:),r2(:)
    INTEGER, OPTIONAL, INTENT(IN) :: ulp
    INTEGER :: i
    DO i = 1, MIN(SIZE(r1),SIZE(r2))
      CALL fp_equal_scalar(r1(i),r2(i),ulp=ulp)
    END DO
  END SUBROUTINE fp_equal_rank1


  SUBROUTINE ip_notequal(i1,i2)
    INTEGER, INTENT(IN) :: i1,i2
    IF ( i1 /= i2 ) THEN
      IF (noisy) THEN
        WRITE(*,'(4x,"Assert not equal passed. Expected ", i0, " and got ", i0 )') i1, i2
      END IF
      CALL test_passed()
    ELSE
      WRITE(*,'(4x,"Assert not equal FAILED. Expected ", i0, " but got ", i0 )') i1, i2
      CALL test_failed()
    END IF
  END SUBROUTINE ip_notequal
  
  SUBROUTINE fp_notequal_scalar(r1,r2, ulp)
    REAL(fp),          INTENT(IN) :: r1,r2
    INTEGER, OPTIONAL, INTENT(IN) :: ulp
    CHARACTER(*), PARAMETER :: RFMT = 'es25.18'
    INTEGER :: rel
    rel = 1
    IF ( PRESENT(ulp) ) rel = ulp
    IF ( .NOT. Compare_Float(r1,r2,ULP=rel) ) THEN
      IF (noisy) THEN
        WRITE(*,'(4x,"Assert not equal passed. Expected ", '//RFMT//', " and got ", '//RFMT//' )') r1,r2
      END IF
      CALL test_passed()
    ELSE
      WRITE(*,'(4x,"Assert not equal FAILED. Expected ", '//RFMT//', " but got ", '//RFMT//' )') r1,r2
      CALL test_failed()
    END IF
  END SUBROUTINE fp_notequal_scalar

  SUBROUTINE fp_notequal_rank1(r1,r2, ulp)
    REAL(fp),          INTENT(IN) :: r1(:),r2(:)
    INTEGER, OPTIONAL, INTENT(IN) :: ulp
    INTEGER :: i
    DO i = 1, MIN(SIZE(r1),SIZE(r2))
      CALL fp_notequal_scalar(r1(i),r2(i),ulp=ulp)
    END DO
  END SUBROUTINE fp_notequal_rank1




  SUBROUTINE init_alltests()
    n_alltests        = 0
    n_passed_alltests = 0
    n_failed_alltests = 0
  END SUBROUTINE init_alltests

  SUBROUTINE init_test(message,verbose)
    CHARACTER(*),      INTENT(IN) :: message
    LOGICAL, OPTIONAL, INTENT(IN) :: verbose
    n_tests        = 0
    n_passed_tests = 0
    n_failed_tests = 0
    noisy = .FALSE.
    WRITE(*,'(2x,a)') TRIM(message)
    IF ( PRESENT(verbose) ) noisy=verbose
  END SUBROUTINE init_test

  SUBROUTINE report_test()
    CHARACTER(10) :: notify
    notify = ''
    IF ( n_failed_tests /= 0 ) notify = '  <----<<<'
    WRITE(*,'(4x,"Passed ",i0," of ",i0," tests")') n_passed_tests, n_tests
    WRITE(*,'(4x,"Failed ",i0," of ",i0," tests",a)') n_failed_tests, n_tests, notify
  END SUBROUTINE report_test

  SUBROUTINE report_alltests()
    CHARACTER(10) :: notify
    notify = ''
    IF ( n_failed_alltests /= 0 ) notify = '  <----<<<'
    WRITE(*,'(/2x,"=======================")')
    WRITE(*,'( 2x,"SUMMARY of test results")')
    WRITE(*,'( 2x,"=======================")')
    WRITE(*,'( 2x,"Passed ",i0," of ",i0," tests")') n_passed_alltests, n_alltests
    WRITE(*,'( 2x,"Failed ",i0," of ",i0," tests",a)') n_failed_alltests, n_alltests, notify
  END SUBROUTINE report_alltests

  ! -------  
  ! Private
  ! -------  
  SUBROUTINE test_passed()
    n_tests        = n_tests + 1
    n_passed_tests = n_passed_tests + 1
    n_alltests        = n_alltests        + 1
    n_passed_alltests = n_passed_alltests + 1
  END SUBROUTINE test_passed
    
  SUBROUTINE test_failed()
    n_tests        = n_tests + 1
    n_failed_tests = n_failed_tests + 1
    n_alltests        = n_alltests        + 1
    n_failed_alltests = n_failed_alltests + 1
  END SUBROUTINE test_failed
    
END MODULE Unit_Test
