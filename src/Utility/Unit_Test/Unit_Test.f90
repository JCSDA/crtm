! Utility for some simple unit testing routines.
!
MODULE Unit_Test

  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds           , ONLY: fp
  USE Message_Handler      , ONLY: INFORMATION, Display_Message
  USE Compare_Float_Numbers

  ! Disable all implicit typing
  IMPLICIT NONE
  
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: UTest_type
  PUBLIC :: Assert
  PUBLIC :: Assert_Equal
  PUBLIC :: Assert_NotEqual
  PUBLIC :: Init_AllTests
  PUBLIC :: Init_Test
  PUBLIC :: Report_Test
  PUBLIC :: Report_AllTests
  PUBLIC :: n_Tests_Passed
  PUBLIC :: n_Tests_Failed

  ! Procedure overloads
  ! -------------------
  INTERFACE Assert_Equal
    MODULE PROCEDURE ip_equal
    MODULE PROCEDURE fp_equal_scalar
    MODULE PROCEDURE fp_equal_rank1
    MODULE PROCEDURE fp_equal_rank2
  END INTERFACE Assert_Equal
  
  INTERFACE Assert_NotEqual
    MODULE PROCEDURE ip_notequal
    MODULE PROCEDURE fp_notequal_scalar
    MODULE PROCEDURE fp_notequal_rank1
  END INTERFACE Assert_NotEqual
  
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id: $'
  CHARACTER(*), PARAMETER :: MODULE_NAME = 'Unit_Test'
  INTEGER,      PARAMETER :: SL = 256
  INTEGER,      PARAMETER :: CR = 13
  INTEGER,      PARAMETER :: LF = 10
  CHARACTER(2), PARAMETER :: CRLF = ACHAR(CR)//ACHAR(LF)
  INTEGER,      PARAMETER :: SET = 1
  CHARACTER(*), PARAMETER :: RFMT = 'es25.18'
  LOGICAL,      PARAMETER :: DEFAULT_NOISY = .FALSE.

  ! Derived type definitions
  ! ------------------------
  TYPE :: UTest_type
    PRIVATE
    ! Test settings
    LOGICAL       :: Verbose = .FALSE.
    CHARACTER(SL) :: Title   = ' '
    CHARACTER(SL) :: Caller  = ' '

    ! Individual test counter
    INTEGER :: n_Tests        = 0
    INTEGER :: n_Passed_Tests = 0
    INTEGER :: n_Failed_Tests = 0
    ! All test counters
    
    INTEGER :: n_AllTests        = 0
    INTEGER :: n_Passed_AllTests = 0
    INTEGER :: n_Failed_AllTests = 0
  END TYPE UTest_type
  
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
!       Assert
!
! PURPOSE:
!       Subroutine to assert its argument
!
! CALLING SEQUENCE:
!       CALL Assert(Test                   , &  ! Input
!                   UTest                  , &  ! Input
!                   Message_Log=Message_Log  )  ! Optional input
!
!------------------------------------------------------------------------------

  SUBROUTINE Assert(Test,UTest,Message_Log)
    ! Arguments
    LOGICAL,                INTENT(IN)     :: Test
    TYPE(UTest_type),       INTENT(IN OUT) :: UTest
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Assert'
    ! Variables
    CHARACTER(256) :: Message
    LOGICAL :: Verbose
    
    ! Default output
    Verbose = UTest%Verbose
    
    ! Check Test
    IF ( Test ) THEN
      CALL test_passed(UTest)
      WRITE( Message,'(4x,"Assert passed for test#",i0)') UTest%n_Tests
    ELSE
      CALL test_failed(UTest)
      Verbose = .TRUE.  ! Always output test failure
      WRITE( Message,'(4x,"Assert FAILED for test#",i0)') UTest%n_Tests
    END IF
    
    ! Output message
    IF ( Verbose ) CALL Display_Message( ROUTINE_NAME, &
                                         TRIM(Message), &
                                         INFORMATION, &
                                         Message_Log=Message_Log )
  END SUBROUTINE Assert
  

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
!                         UTest                  , &  ! Input
!                         ULP        =ULP        , &  ! Optional input
!                         Percent    =Percent    , &  ! Optional input
!                         Message_Log=Message_Log  )  ! Optional input
!
!------------------------------------------------------------------------------

  SUBROUTINE ip_equal(i1,i2,UTest,Message_Log)
    ! Arguments
    INTEGER,                INTENT(IN)     :: i1,i2
    TYPE(UTest_type),       INTENT(IN OUT) :: UTest
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Assert_Equal'
    ! Variables
    CHARACTER(256) :: Message
    LOGICAL :: Verbose
    
    ! Default output
    Verbose = UTest%Verbose
    
    ! Check equality
    IF ( i1 == i2 ) THEN
      CALL test_passed(UTest)
      WRITE( Message,'(4x,"Assert equal passed for test#",i0,&
                     &". Expected ",i0," and got ",i0)') UTest%n_Tests, i1, i2
    ELSE
      CALL test_failed(UTest)
      Verbose = .TRUE.  ! Always output test failure
      WRITE( Message,'(4x,"Assert equal FAILED for test#",i0,&
                     &". Expected ",i0," and got ",i0)') UTest%n_Tests, i1, i2
    END IF
    
    ! Output message
    IF ( Verbose ) CALL Display_Message( ROUTINE_NAME, &
                                         TRIM(Message), &
                                         INFORMATION, &
                                         Message_Log=Message_Log )
  END SUBROUTINE ip_equal
  
  SUBROUTINE fp_equal_scalar(r1,r2,UTest,ULP,Percent,Message_Log)
    ! Arguments
    REAL(fp),               INTENT(IN)     :: r1,r2
    TYPE(UTest_type),       INTENT(IN OUT) :: UTest
    INTEGER,      OPTIONAL, INTENT(IN)     :: ULP
    REAL(fp),     OPTIONAL, INTENT(IN)     :: Percent
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Assert_Equal'
    ! Variables
    CHARACTER(256) :: Message
    LOGICAL  :: Verbose
    INTEGER  :: rel
    REAL(fp) :: pc

    ! Minimum error by default...
    rel = 1
    ! ...unless ULP argument is passed
    IF ( PRESENT(ULP) ) rel = ABS(ULP)

    ! Default output
    Verbose = UTest%Verbose

    ! Check equality
    IF ( Compare_Float(r1,r2,ULP=rel,Percent=Percent) ) THEN
      CALL test_passed(UTest)
      WRITE( Message, '(4x,"Assert equal passed for test#",i0,&
                      &". Expected ",'//RFMT//'," and got ",'//RFMT//')') UTest%n_Tests, r1,r2
    ELSE
      CALL test_failed(UTest)
      Verbose = .TRUE.  ! Always output test failure
      WRITE( Message, '(4x,"Assert equal FAILED for test#",i0,&
                      &". Expected ",'//RFMT//'," and got ",'//RFMT//')') UTest%n_Tests, r1,r2
    END IF

    ! Output message
    IF ( Verbose ) CALL Display_Message( ROUTINE_NAME, &
                                         TRIM(Message), &
                                         INFORMATION, &
                                         Message_Log=Message_Log )
  END SUBROUTINE fp_equal_scalar

  SUBROUTINE fp_equal_rank1(r1,r2,UTest,ULP,Percent,Message_Log)
    ! Arguments
    REAL(fp),               INTENT(IN)     :: r1(:),r2(:)
    TYPE(UTest_type),       INTENT(IN OUT) :: UTest
    INTEGER,      OPTIONAL, INTENT(IN)     :: ULP
    REAL(fp),     OPTIONAL, INTENT(IN)     :: Percent
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Variables
    INTEGER :: i
    
    ! Loop over elements
    DO i = 1, MIN(SIZE(r1),SIZE(r2))
      CALL fp_equal_scalar(r1(i),r2(i),UTest,ULP=ULP,Percent=Percent,Message_Log=Message_Log)
    END DO
  END SUBROUTINE fp_equal_rank1


  SUBROUTINE fp_equal_rank2(r1,r2,UTest,ULP,Percent,Message_Log)
    ! Arguments
    REAL(fp),               INTENT(IN)     :: r1(:,:),r2(:,:)
    TYPE(UTest_type),       INTENT(IN OUT) :: UTest
    INTEGER,      OPTIONAL, INTENT(IN)     :: ULP
    REAL(fp),     OPTIONAL, INTENT(IN)     :: Percent
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Variables
    INTEGER :: i, j, m, n
    
    m = MIN(SIZE(r1,DIM=1),SIZE(r2,DIM=1))
    n = MIN(SIZE(r1,DIM=2),SIZE(r2,DIM=2))
    
    ! Loop over elements
    DO j = 1, n
      DO i = 1, m
        CALL fp_equal_scalar(r1(i,j),r2(i,j),UTest,ULP=ULP,Percent=Percent,Message_Log=Message_Log)
      END DO
    END DO
  END SUBROUTINE fp_equal_rank2


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
!                            UTest                  , &  ! Input
!                            ULP        =ULP        , &  ! Optional input
!                            Percent    =Percent    , &  ! Optional input
!                            Message_Log=Message_Log  )  ! Optional input
!
!------------------------------------------------------------------------------

  SUBROUTINE ip_notequal(i1,i2,UTest,Message_Log)
    ! Arguments
    INTEGER,                INTENT(IN)     :: i1,i2
    TYPE(UTest_type),       INTENT(IN OUT) :: UTest
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Assert_Equal'
    ! Variables
    CHARACTER(256) :: Message
    LOGICAL :: Verbose
    
    ! Default output
    Verbose = UTest%Verbose

    ! Check inequality    
    IF ( i1 /= i2 ) THEN
      CALL test_passed(UTest)
      WRITE( Message, '(4x,"Assert not equal passed for test#",i0,&
                     &". Expected ",i0," and got ",i0)') UTest%n_Tests, i1, i2
    ELSE
      CALL test_failed(UTest)
      Verbose = .TRUE.  ! Always output test failure
      WRITE( Message, '(4x,"Assert not equal FAILED for test#",i0,&
                     &". Expected ",i0," and got ",i0)') UTest%n_Tests, i1, i2
    END IF
    
    ! Output message
    IF ( Verbose ) CALL Display_Message( ROUTINE_NAME, &
                                         TRIM(MEssage), &
                                         INFORMATION, &
                                         Message_Log=Message_Log )
  END SUBROUTINE ip_notequal
  
  SUBROUTINE fp_notequal_scalar(r1,r2,UTest,ULP,Percent,Message_Log)
    ! Arguments
    REAL(fp),               INTENT(IN)     :: r1,r2
    TYPE(UTest_type),       INTENT(IN OUT) :: UTest
    INTEGER,      OPTIONAL, INTENT(IN)     :: ULP
    REAL(fp),     OPTIONAL, INTENT(IN)     :: Percent
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
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
    Verbose = UTest%Verbose

    ! Check inequality    
    IF ( .NOT. Compare_Float(r1,r2,ULP=rel,Percent=Percent) ) THEN
      CALL test_passed(UTest)
      WRITE( Message, '(4x,"Assert not equal passed for test#",i0,&
                      &". Expected ",'//RFMT//'," and got ",'//RFMT//')') UTest%n_Tests, r1,r2
    ELSE
      CALL test_failed(UTest)
      Verbose = .TRUE.  ! Always output test failure
      WRITE( Message, '(4x,"Assert not equal FAILED for test#",i0,&
                      &". Expected ",'//RFMT//'," and got ",'//RFMT//')') UTest%n_Tests, r1,r2
    END IF

    ! Output message
    IF ( Verbose ) CALL Display_Message( ROUTINE_NAME, &
                                         TRIM(Message), &
                                         INFORMATION, &
                                         Message_Log=Message_Log )
  END SUBROUTINE fp_notequal_scalar

  SUBROUTINE fp_notequal_rank1(r1,r2,UTest,ULP,Percent,Message_Log)
    ! Arguments
    REAL(fp),               INTENT(IN)     :: r1(:),r2(:)
    TYPE(UTest_type),       INTENT(IN OUT) :: UTest
    INTEGER,      OPTIONAL, INTENT(IN)     :: ULP
    REAL(fp),     OPTIONAL, INTENT(IN)     :: Percent
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Variables
    INTEGER :: i
    
    ! Loop over elements
    DO i = 1, MIN(SIZE(r1),SIZE(r2))
      CALL fp_notequal_scalar(r1(i),r2(i),UTest,ULP=ULP,Percent=Percent,Message_Log=Message_Log)
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
!       CALL Init_AllTests(UTest)
!
!------------------------------------------------------------------------------

  SUBROUTINE Init_AllTests(UTest)
    TYPE(UTest_type), INTENT(OUT) :: UTest
    UTest%n_Tests        = 0
    UTest%n_Passed_Tests = 0
    UTest%n_Failed_Tests = 0
    UTest%n_AllTests        = 0
    UTest%n_Passed_AllTests = 0
    UTest%n_Failed_AllTests = 0
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
!       CALL Init_Test(UTest                 , &  ! Input
!                      Title                 , &  ! Input
!                      Caller     =Caller    , &  ! Optional input
!                      Verbose    =Verbose   , &  ! Optional input
!                      Message_Log=MessageLog  )  ! Optional input
!
!------------------------------------------------------------------------------

  SUBROUTINE Init_Test(UTest, Title, Caller, Verbose, Message_Log)
    ! Arguments
    TYPE(UTest_type)      , INTENT(IN OUT) :: UTest
    CHARACTER(*)          , INTENT(IN)     :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Caller
    LOGICAL,      OPTIONAL, INTENT(IN)     :: Verbose
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Variables
    CHARACTER(256) :: Routine_Name
        
    ! Initialise test counters
    UTest%n_Tests        = 0
    UTest%n_Passed_Tests = 0
    UTest%n_Failed_Tests = 0
    
    ! Set test title
    UTest%Title = ADJUSTL(Title)
    
    ! Default name if no caller
    UTest%Caller = 'Init_Test'
    IF ( PRESENT(Caller) ) UTest%Caller = TRIM(ADJUSTL(Caller))
    
    ! Set output selection
    UTest%Verbose = DEFAULT_NOISY
    IF ( PRESENT(Verbose) ) UTest%Verbose=Verbose
    
    ! Output test title
    CALL Display_Message( TRIM(UTest%Caller), &
                          'TEST: '//TRIM(UTest%Title), &
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
!       CALL Report_Test( UTest,                 &  ! Optional input
!                         Message_Log=MessageLog )  ! Optional input
!
!------------------------------------------------------------------------------

  SUBROUTINE Report_Test(UTest,Message_Log)
    ! Arguments
    TYPE(UTest_type)      , INTENT(IN) :: UTest
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Message_Log
    ! Variables
    CHARACTER(256) :: Message
    CHARACTER(256) :: Attention
    
    ! Test fail attention-grabber
    Attention = ''
    IF ( UTest%n_Failed_Tests /= 0 ) Attention = '  <----<<<  **WARNING**'

    ! Output results
    WRITE( Message, '(a,4x,"Passed ",i0," of ",i0," tests", &
                     &a,4x,"Failed ",i0," of ",i0," tests",a)') &
                     CRLF, &
                     UTest%n_Passed_Tests, UTest%n_Tests, &
                     CRLF, &
                     UTest%n_Failed_Tests, UTest%n_Tests, TRIM(Attention)
    CALL Display_Message(TRIM(UTest%Caller), &
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
!       CALL Report_AllTests( UTest,                 &  ! Optional input
!                             Message_Log=MessageLog )  ! Optional input
!
!------------------------------------------------------------------------------

  SUBROUTINE Report_AllTests(UTest,Message_Log)
    ! Arguments
    TYPE(UTest_type)      , INTENT(IN) :: UTest
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Message_Log
    ! Variables
    CHARACTER(256) :: Message
    CHARACTER(256) :: Attention
    
    ! Test fail attention-grabber
    Attention = ''
    IF ( UTest%n_Failed_AllTests /= 0 ) Attention = '  <----<<<  **WARNING**'

    ! Output results
    WRITE( Message,'(a,2x,"=======================",&
                    &a,2x,"SUMMARY of test results",&
                    &a,2x,"=======================",&
                    &a,2x,"Passed ",i0," of ",i0," tests",&
                    &a,2x,"Failed ",i0," of ",i0," tests",a)') &
                    CRLF,CRLF,CRLF,CRLF, &
                    UTest%n_Passed_AllTests, UTest%n_AllTests, &
                    CRLF, &
                    UTest%n_Failed_AllTests, UTest%n_AllTests, &
                    TRIM(Attention)
    CALL Display_Message(TRIM(UTest%Caller), &
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
!       n = n_Tests_Passed(UTest)
!
!------------------------------------------------------------------------------

  PURE FUNCTION n_Tests_Passed(UTest) RESULT(n)
    TYPE(UTest_type), INTENT(IN) :: UTest
    INTEGER :: n
    n = UTest%n_Passed_AllTests
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
!       n = n_Tests_Passed(UTest)
!
!------------------------------------------------------------------------------

  PURE FUNCTION n_Tests_Failed(UTest) RESULT(n)
    TYPE(UTest_type), INTENT(IN) :: UTest
    INTEGER :: n
    n = UTest%n_Failed_AllTests
  END FUNCTION n_Tests_Failed
  

  
!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################
  
  SUBROUTINE test_passed(UTest)
    TYPE(UTest_type), INTENT(IN OUT) :: UTest
    UTest%n_Tests        = UTest%n_Tests        + 1
    UTest%n_Passed_Tests = UTest%n_Passed_Tests + 1
    UTest%n_AllTests        = UTest%n_AllTests        + 1
    UTest%n_Passed_AllTests = UTest%n_Passed_AllTests + 1
  END SUBROUTINE test_passed
    
  SUBROUTINE test_failed(UTest)
    TYPE(UTest_type), INTENT(IN OUT) :: UTest
    UTest%n_Tests        = UTest%n_Tests        + 1
    UTest%n_Failed_Tests = UTest%n_Failed_Tests + 1
    UTest%n_AllTests        = UTest%n_AllTests        + 1
    UTest%n_Failed_AllTests = UTest%n_Failed_AllTests + 1
  END SUBROUTINE test_failed
    
END MODULE Unit_Test
