! Utility for some simple unit testing routines.
!
MODULE Unit_Test

  ! Module usage
  USE Type_Kinds           , ONLY: fp
  USE Message_Handler      , ONLY: INFORMATION, Display_Message
  USE Compare_Float_Numbers

  ! Disable all implicit typing
  IMPLICIT NONE
  
  ! Visibilities
  PRIVATE
  PUBLIC :: Assert_Equal
  PUBLIC :: Assert_NotEqual
  PUBLIC :: Init_AllTests
  PUBLIC :: Init_Test
  PUBLIC :: Report_Test
  PUBLIC :: Report_AllTests
  PUBLIC :: n_Tests_Passed
  PUBLIC :: n_Tests_Failed

  ! Procedure overloads
  INTERFACE Assert_Equal
    MODULE PROCEDURE ip_equal
    MODULE PROCEDURE fp_equal_scalar
    MODULE PROCEDURE fp_equal_rank1
  END INTERFACE Assert_Equal
  
  INTERFACE Assert_NotEqual
    MODULE PROCEDURE ip_notequal
    MODULE PROCEDURE fp_notequal_scalar
    MODULE PROCEDURE fp_notequal_rank1
  END INTERFACE Assert_NotEqual
  
  ! Module parameters
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id: $'
  CHARACTER(*), PARAMETER :: MODULE_NAME = 'Unit_Test'
  INTEGER,      PARAMETER :: CR = 13
  INTEGER,      PARAMETER :: LF = 10
  CHARACTER(2), PARAMETER :: CRLF = ACHAR(CR)//ACHAR(LF)
  INTEGER,      PARAMETER :: SET = 1
  CHARACTER(*), PARAMETER :: RFMT = 'es25.18'
  LOGICAL,      PARAMETER :: DEFAULT_NOISY = .FALSE.

  ! Module variables
  INTEGER, SAVE :: n_Tests     , n_Passed_Tests     , n_Failed_Tests
  INTEGER, SAVE :: n_AllTests=0, n_Passed_AllTests=0, n_Failed_AllTests=0
  LOGICAL, SAVE :: Noisy = DEFAULT_NOISY

CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!------------------------------------------------------------------------------
!
! NAME:
!       Assert_Equal
!
! PURPOSE:
!       Subroutine to assert that two arguments are equal
!
! CALLING SEQUENCE:
!       CALL Assert_Equal(v1, v2                 , &  ! Input
!                         ULP        =ULP        , &  ! Optional input
!                         Message_Log=Message_Log  )  ! Optional input
!
!------------------------------------------------------------------------------

  SUBROUTINE ip_equal(i1,i2,Message_Log)
    ! Arguments
    INTEGER,                INTENT(IN) :: i1,i2
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Message_Log
    ! Parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Assert_Equal'
    ! Variables
    CHARACTER(256) :: Message
    LOGICAL :: Verbose
    
    ! Default output
    Verbose = Noisy
    
    ! Check equality
    IF ( i1 == i2 ) THEN
      WRITE( Message,'(4x,"Assert equal passed. Expected ", i0, " and got ", i0 )') i1, i2
      CALL test_passed()
    ELSE
      Verbose = .TRUE.  ! Always output test failure
      WRITE( Message,'(4x,"Assert equal FAILED. Expected ", i0, " but got ", i0 )') i1, i2
      CALL test_failed()
    END IF
    
    ! Output message
    IF ( Verbose ) CALL Display_Message( ROUTINE_NAME, &
                                         TRIM(MEssage), &
                                         INFORMATION, &
                                         Message_Log=Message_Log )
  END SUBROUTINE ip_equal
  
  SUBROUTINE fp_equal_scalar(r1,r2,ULP,Message_Log)
    ! Arguments
    REAL(fp),               INTENT(IN) :: r1,r2
    INTEGER,      OPTIONAL, INTENT(IN) :: ULP
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Message_Log
    ! Parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Assert_Equal'
    ! Variables
    CHARACTER(256) :: Message
    LOGICAL :: Verbose
    INTEGER :: rel

    ! Minimum error by default...
    rel = 1
    ! ...unless ULP argument is passed
    IF ( PRESENT(ULP) ) rel = ABS(ULP)

    ! Default output
    Verbose = Noisy

    ! Check equality
    IF ( Compare_Float(r1,r2,ULP=rel) ) THEN
      WRITE( Message, '(4x,"Assert equal passed. Expected ", '//RFMT//', " and got ", '//RFMT//' )') r1,r2
      CALL test_passed()
    ELSE
      Verbose = .TRUE.  ! Always output test failure
      WRITE( Message, '(4x,"Assert equal FAILED. Expected ", '//RFMT//', " but got ", '//RFMT//' )') r1,r2
      CALL test_failed()
    END IF

    ! Output message
    IF ( Verbose ) CALL Display_Message( ROUTINE_NAME, &
                                         TRIM(MEssage), &
                                         INFORMATION, &
                                         Message_Log=Message_Log )
  END SUBROUTINE fp_equal_scalar

  SUBROUTINE fp_equal_rank1(r1,r2,ULP,Message_Log)
    ! Arguments
    REAL(fp),               INTENT(IN) :: r1(:),r2(:)
    INTEGER,      OPTIONAL, INTENT(IN) :: ULP
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Message_Log
    ! Variables
    INTEGER :: i
    
    ! Loop over elements
    DO i = 1, MIN(SIZE(r1),SIZE(r2))
      CALL fp_equal_scalar(r1(i),r2(i),ULP=ULP,Message_Log=Message_Log)
    END DO
  END SUBROUTINE fp_equal_rank1


!------------------------------------------------------------------------------
!
! NAME:
!       Assert_NotEqual
!
! PURPOSE:
!       Subroutine to assert that two arguments are NOT equal
!
! CALLING SEQUENCE:
!       CALL Assert_NotEqual(v1, v2                 , &  ! Input
!                            ULP        =ULP        , &  ! Optional input
!                            Message_Log=Message_Log  )  ! Optional input
!
!------------------------------------------------------------------------------

  SUBROUTINE ip_notequal(i1,i2,Message_Log)
    ! Arguments
    INTEGER,                INTENT(IN) :: i1,i2
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Message_Log
    ! Parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Assert_Equal'
    ! Variables
    CHARACTER(256) :: Message
    LOGICAL :: Verbose
    
    ! Default output
    Verbose = Noisy

    ! Check inequality    
    IF ( i1 /= i2 ) THEN
      WRITE( Message, '(4x,"Assert not equal passed. Expected ", i0, " and got ", i0 )') i1, i2
      CALL test_passed()
    ELSE
      Verbose = .TRUE.  ! Always output test failure
      WRITE( Message, '(4x,"Assert not equal FAILED. Expected ", i0, " but got ", i0 )') i1, i2
      CALL test_failed()
    END IF
    
    ! Output message
    IF ( Verbose ) CALL Display_Message( ROUTINE_NAME, &
                                         TRIM(MEssage), &
                                         INFORMATION, &
                                         Message_Log=Message_Log )
  END SUBROUTINE ip_notequal
  
  SUBROUTINE fp_notequal_scalar(r1,r2,ULP,Message_Log)
    ! Arguments
    REAL(fp),               INTENT(IN) :: r1,r2
    INTEGER,      OPTIONAL, INTENT(IN) :: ULP
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Message_Log
    ! Parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Assert_Equal'
    ! Variables
    CHARACTER(256) :: Message
    LOGICAL :: Verbose
    INTEGER :: rel

    ! Minimum error by default...
    rel = 1
    ! ...unless ULP argument is passed
    IF ( PRESENT(ULP) ) rel = ABS(ULP)

    ! Default output
    Verbose = Noisy

    ! Check inequality    
    IF ( .NOT. Compare_Float(r1,r2,ULP=rel) ) THEN
      WRITE( Message, '(4x,"Assert not equal passed. Expected ", '//RFMT//', " and got ", '//RFMT//' )') r1,r2
      CALL test_passed()
    ELSE
      Verbose = .TRUE.  ! Always output test failure
      WRITE( Message, '(4x,"Assert not equal FAILED. Expected ", '//RFMT//', " but got ", '//RFMT//' )') r1,r2
      CALL test_failed()
    END IF

    ! Output message
    IF ( Verbose ) CALL Display_Message( ROUTINE_NAME, &
                                         TRIM(Message), &
                                         INFORMATION, &
                                         Message_Log=Message_Log )
  END SUBROUTINE fp_notequal_scalar

  SUBROUTINE fp_notequal_rank1(r1,r2,ULP,Message_Log)
    ! Arguments
    REAL(fp),               INTENT(IN) :: r1(:),r2(:)
    INTEGER,      OPTIONAL, INTENT(IN) :: ULP
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Message_Log
    ! Variables
    INTEGER :: i
    
    ! Loop over elements
    DO i = 1, MIN(SIZE(r1),SIZE(r2))
      CALL fp_notequal_scalar(r1(i),r2(i),ULP=ULP,Message_Log=Message_Log)
    END DO
  END SUBROUTINE fp_notequal_rank1


!------------------------------------------------------------------------------
!
! NAME:
!       Init_AllTests
!
! PURPOSE:
!       Test suite initialisation subroutine
!
! CALLING SEQUENCE:
!       CALL Init_AllTests()
!
!------------------------------------------------------------------------------

  SUBROUTINE Init_AllTests()
    n_AllTests        = 0
    n_Passed_AllTests = 0
    n_Failed_AllTests = 0
  END SUBROUTINE Init_AllTests


!------------------------------------------------------------------------------
!
! NAME:
!       Init_Test
!
! PURPOSE:
!       Individual test initialisation subroutine
!
! CALLING SEQUENCE:
!       CALL Init_Test(Title                 , &  ! Input
!                      Caller     =Caller    , &  ! Optional input
!                      Verbose    =Verbose   , &  ! Optional input
!                      Message_Log=MessageLog  )  ! Optional input
!
!------------------------------------------------------------------------------

  SUBROUTINE Init_Test(Title,Caller,Verbose,Message_Log)
    ! Arguments
    CHARACTER(*),           INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Caller
    LOGICAL,      OPTIONAL, INTENT(IN) :: Verbose
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Message_Log
    ! Variables
    CHARACTER(256) :: Routine_Name
        
    ! Initialise test counters
    n_Tests        = 0
    n_Passed_Tests = 0
    n_Failed_Tests = 0
    
    ! Default name if no caller
    Routine_Name = 'Init_Test'
    IF ( PRESENT(Caller) ) Routine_Name = TRIM(ADJUSTL(Caller))
    
    ! Set output selection
    Noisy = DEFAULT_NOISY
    IF ( PRESENT(Verbose) ) Noisy=Verbose
    
    ! Output test title
    CALL Display_Message( TRIM(Routine_Name), &
                          'TEST: '//TRIM(Title), &
                          INFORMATION, &
                          Message_Log=Message_Log )
  END SUBROUTINE Init_Test


!------------------------------------------------------------------------------
!
! NAME:
!       Report_Test
!
! PURPOSE:
!       Individual test report subroutine
!
! CALLING SEQUENCE:
!       CALL Report_Test( Caller     =Caller    , &  ! Optional input
!                         Message_Log=MessageLog  )  ! Optional input
!
!------------------------------------------------------------------------------

  SUBROUTINE Report_Test(Caller,Message_Log)
    ! Arguments
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Caller
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Message_Log
    ! Variables
    CHARACTER(256) :: Message
    CHARACTER(256) :: Routine_Name
    CHARACTER(10)  :: Attention
    
    ! Default name if no caller
    Routine_Name = 'Report_Test'
    IF ( PRESENT(Caller) ) Routine_Name = TRIM(ADJUSTL(Caller))
    
    ! Test fail attention-grabber
    Attention = ''
    IF ( n_Failed_Tests /= 0 ) Attention = '  <----<<<'

    ! Output results
    WRITE( Message, '(a,4x,"Passed ",i0," of ",i0," tests", &
                     &a,4x,"Failed ",i0," of ",i0," tests",a)') &
                     CRLF, &
                     n_Passed_Tests, n_Tests, &
                     CRLF, &
                     n_Failed_Tests, n_Tests, Attention
    CALL Display_Message(TRIM(Routine_Name), &
                         TRIM(Message), &
                         INFORMATION, &
                         Message_Log=Message_Log )
  END SUBROUTINE Report_Test


!------------------------------------------------------------------------------
!
! NAME:
!       Report_AllTests
!
! PURPOSE:
!       Test suite report subroutine
!
! CALLING SEQUENCE:
!       CALL Report_AllTests( Caller     =Caller    , &  ! Optional input
!                             Message_Log=MessageLog  )  ! Optional input
!
!------------------------------------------------------------------------------

  SUBROUTINE Report_AllTests(Caller,Message_Log)
    ! Arguments
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Caller
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Message_Log
    ! Variables
    CHARACTER(256) :: Message
    CHARACTER(256) :: Routine_Name
    CHARACTER(10)  :: Attention
    
    ! Default name if no caller
    Routine_Name = 'Report_AllTests'
    IF ( PRESENT(Caller) ) Routine_Name = TRIM(ADJUSTL(Caller))
    
    ! Test fail attention-grabber
    Attention = ''
    IF ( n_Failed_Tests /= 0 ) Attention = '  <----<<<'

    ! Output results
    WRITE( Message,'(a,2x,"=======================",&
                    &a,2x,"SUMMARY of test results",&
                    &a,2x,"=======================",&
                    &a,2x,"Passed ",i0," of ",i0," tests",&
                    &a,2x,"Failed ",i0," of ",i0," tests",a)') &
                    CRLF,CRLF,CRLF,CRLF, &
                    n_Passed_AllTests, n_AllTests, &
                    CRLF, &
                    n_Failed_AllTests, n_AllTests, &
                    Attention
    CALL Display_Message(TRIM(Routine_Name), &
                         TRIM(Message), &
                         INFORMATION, &
                         Message_Log=Message_Log )
  END SUBROUTINE Report_AllTests


!------------------------------------------------------------------------------
!
! NAME:
!       n_Tests_Passed
!
! PURPOSE:
!       Function to return the number of tests passed
!
! CALLING SEQUENCE:
!       n = n_Tests_Passed()
!
!------------------------------------------------------------------------------

  PURE FUNCTION n_Tests_Passed() RESULT(n)
    INTEGER :: n
    n = n_Passed_AllTests
  END FUNCTION n_Tests_Passed


!------------------------------------------------------------------------------
!
! NAME:
!       n_Tests_Passed
!
! PURPOSE:
!       Function to return the number of tests passed
!
! CALLING SEQUENCE:
!       n = n_Tests_Passed()
!
!------------------------------------------------------------------------------

  PURE FUNCTION n_Tests_Failed() RESULT(n)
    INTEGER :: n
    n = n_Failed_AllTests
  END FUNCTION n_Tests_Failed
  

  
!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################
  
  SUBROUTINE test_passed()
    n_Tests        = n_Tests + 1
    n_Passed_Tests = n_Passed_Tests + 1
    n_AllTests        = n_AllTests        + 1
    n_Passed_AllTests = n_Passed_AllTests + 1
  END SUBROUTINE test_passed
    
  SUBROUTINE test_failed()
    n_Tests        = n_Tests + 1
    n_Failed_Tests = n_Failed_Tests + 1
    n_AllTests        = n_AllTests        + 1
    n_Failed_AllTests = n_Failed_AllTests + 1
  END SUBROUTINE test_failed
    
END MODULE Unit_Test
