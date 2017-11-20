#! /usr/bin/env ruby

# Brain dead script to create the assert_withinsigfig and refute_withinsigfig
# procedures for scalar, rank-1, and rank-2 inputs

  def create_scalar(testinfo, typeinfo)
    src=""
    if typeinfo[:type] == "COMPLEX" then
      src=<<-EOF
  SUBROUTINE #{typeinfo[:tag]}_#{testinfo[:type]}_withinsigfig_s( self, Expected, Actual, n_SigFig )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    #{typeinfo[:type]}(#{typeinfo[:kind]}), INTENT(IN) :: Expected, Actual
    INTEGER, INTENT(IN) :: n_SigFig
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::#{testinfo[:type].capitalize}_WithinSigfig[#{typeinfo[:type]}(#{typeinfo[:kind]})]'
    ! Variables
    REAL(#{typeinfo[:kind]}) :: ezr, ezi
    REAL(#{typeinfo[:kind]}) :: azr, azi
    REAL(#{typeinfo[:kind]}) :: epsilon_delta_r, epsilon_delta_i
    LOGICAL :: test
    LOGICAL :: verbose
    CHARACTER(SL) :: message
    ! Setup
    ! ...Split expected into real and imag
    ezr = REAL(Expected,#{typeinfo[:kind]})
    ezi = AIMAG(Expected)
    azr = REAL(Actual,#{typeinfo[:kind]})
    azi = AIMAG(Actual)
    ! ...Compute the test cutoffs
    epsilon_delta_r = EPSILON(ezr) * REAL(RADIX(ezr),#{typeinfo[:kind]})**(EXPONENT(ezr)-1)
    epsilon_delta_i = EPSILON(ezi) * REAL(RADIX(ezi),#{typeinfo[:kind]})**(EXPONENT(ezi)-1)
    ! ...Assign the test
    test = #{testinfo[:unary]}(Compares_Within_Tolerance(ezr, azr, n_SigFig, cutoff=epsilon_delta_r) .AND. &
           Compares_Within_Tolerance(ezi, azi, n_SigFig, cutoff=epsilon_delta_i))
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      self, &
      Verbose = verbose )
    verbose = verbose .OR. (.NOT. test)  ! Always output test failure
    ! Assert the test
    IF ( test ) THEN
      CALL Test_Passed( self )
    ELSE
      CALL Test_Failed( self )
    END IF
    ! Generate the test message
    WRITE( Message, &
      '(a,7x,"Expected     : ",#{typeinfo[:fmt]},a,&
         &7x,"#{testinfo[:msg]}   : ",i0," significant figures",a,&
         &7x,"And got      : ",#{typeinfo[:fmt]},a,&
         &7x,"|Difference| : ",#{typeinfo[:fmt]})') &
      CRLF, Expected, CRLF, n_SigFig, CRLF, Actual, CRLF, CMPLX(ezr-azr,ezi-azi,Single)
    ! Load the object with the message
    CALL Set_Property( &
      self, &
      Level     = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message   = message )
    ! Output the result
    IF ( verbose ) CALL Display_Message( self )
  END SUBROUTINE #{typeinfo[:tag]}_#{testinfo[:type]}_withinsigfig_s
  
  
      EOF
    else
      src=<<-EOF
  SUBROUTINE #{typeinfo[:tag]}_#{testinfo[:type]}_withinsigfig_s( self, Expected, Actual, n_SigFig )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    #{typeinfo[:type]}(#{typeinfo[:kind]}), INTENT(IN) :: Expected, Actual
    INTEGER, INTENT(IN) :: n_SigFig
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::#{testinfo[:type].capitalize}_WithinSigfig[#{typeinfo[:type]}(#{typeinfo[:kind]})]'
    ! Variables
    #{typeinfo[:type]}(#{typeinfo[:kind]}) :: epsilon_delta
    LOGICAL :: test
    LOGICAL :: verbose
    CHARACTER(SL) :: message
    ! Setup
    ! ...Compute the test cutoff
    epsilon_delta = EPSILON(Expected) * REAL(RADIX(Expected),#{typeinfo[:kind]})**(EXPONENT(Expected)-1)
    ! ...Assign the test
    test = #{testinfo[:unary]}(Compares_Within_Tolerance(Expected, Actual, n_SigFig, cutoff=epsilon_delta))
    ! ...Locally modify properties for this test
    CALL Get_Property( &
      self, &
      Verbose = verbose )
    verbose = verbose .OR. (.NOT. test)  ! Always output test failure
    ! Assert the test
    IF ( test ) THEN
      CALL Test_Passed( self )
    ELSE
      CALL Test_Failed( self )
    END IF
    ! Generate the test message
    WRITE( Message, &
      '(a,7x,"Expected     : ",#{typeinfo[:fmt]},a,&
         &7x,"#{testinfo[:msg]}   : ",i0," significant figures",a,&
         &7x,"And got      : ",#{typeinfo[:fmt]},a,&
         &7x,"|Difference| : ",#{typeinfo[:fmt]})') &
      CRLF, Expected, CRLF, n_SigFig, CRLF, Actual, CRLF, ABS(Expected-Actual)
    ! Load the object with the message
    CALL Set_Property( &
      self, &
      Level     = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message   = message )
    ! Output the result
    IF ( verbose ) CALL Display_Message( self )
  END SUBROUTINE #{typeinfo[:tag]}_#{testinfo[:type]}_withinsigfig_s
  
  
      EOF
    end
    src
  end

  def create_rank1(testinfo, typeinfo)
    src=<<-EOF
  SUBROUTINE #{typeinfo[:tag]}_#{testinfo[:type]}_withinsigfig_r1( self, Expected, Actual, n_SigFig )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    #{typeinfo[:type]}(#{typeinfo[:kind]}), INTENT(IN) :: Expected(:), Actual(:)
    INTEGER, INTENT(IN) :: n_SigFig
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::#{testinfo[:type].capitalize}_WithinSigfig[#{typeinfo[:type]}(#{typeinfo[:kind]})]'
    ! Variables
    INTEGER :: i, isize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected)
    IF ( SIZE(Actual) /= isize ) THEN
      CALL Test_Failed( self )
      WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
                     isize, SIZE(Actual)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO i = 1, isize
      CALL self%#{testinfo[:type].capitalize}_WithinSigfig( Expected(i), Actual(i), n_SigFig )
    END DO
  END SUBROUTINE #{typeinfo[:tag]}_#{testinfo[:type]}_withinsigfig_r1
  
  
    EOF
    src
  end

  def create_rank2(testinfo, typeinfo)
    src=<<-EOF
  SUBROUTINE #{typeinfo[:tag]}_#{testinfo[:type]}_withinsigfig_r2( self, Expected, Actual, n_SigFig )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    #{typeinfo[:type]}(#{typeinfo[:kind]}), INTENT(IN) :: Expected(:,:), Actual(:,:)
    INTEGER, INTENT(IN) :: n_SigFig
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::#{testinfo[:type].capitalize}_WithinSigfig[#{typeinfo[:type]}(#{typeinfo[:kind]})]'
    ! Variables
    INTEGER :: i, j, isize, jsize
    CHARACTER(SL) :: Message
    ! Check array sizes
    isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
    IF ( SIZE(Actual,DIM=1) /= isize .OR. &
         SIZE(Actual,DIM=2) /= jsize ) THEN
      CALL Test_Failed( self )
      WRITE( Message, &
        '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
        isize, jsize, &
        SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
      CALL Set_Property( &
        self, &
        Level     = TEST_LEVEL, &
        Procedure = PROCEDURE_NAME, &
        Message   = Message )
      CALL Display_Message( self )
      RETURN
    ENDIF
    ! Loop over elements
    DO j = 1, jsize
      DO i = 1, isize
        CALL self%#{testinfo[:type].capitalize}_WithinSigfig( Expected(i,j), Actual(i,j), n_SigFig )
      END DO
    END DO
  END SUBROUTINE #{typeinfo[:tag]}_#{testinfo[:type]}_withinsigfig_r2
  
  
    EOF
    src
  end

  def create_proclist(testinfo,typeinfo)
    src=""
    testinfo.each do |a|
      typeinfo.each do |b|
        thisproc=<<-EOF
    PROCEDURE, PASS(self) :: #{b[:tag]}_#{a[:type]}_withinsigfig_s
    PROCEDURE, PASS(self) :: #{b[:tag]}_#{a[:type]}_withinsigfig_r1
    PROCEDURE, PASS(self) :: #{b[:tag]}_#{a[:type]}_withinsigfig_r2
        EOF
        src << thisproc
      end
    end
    src << "\n\n\n\n" 
  end
  
  def create_genlist(testinfo,typeinfo)
    src=""
    testinfo.each do |a|
      thistest=<<-EOF
    GENERIC, PUBLIC :: #{a[:type].capitalize}_WithinSigfig => &
      EOF
      src << thistest
      typeinfo.each do |b|
        thisproc=<<-EOF
      #{b[:tag]}_#{a[:type]}_withinsigfig_s, #{b[:tag]}_#{a[:type]}_withinsigfig_r1, #{b[:tag]}_#{a[:type]}_withinsigfig_r2, &
        EOF
        src << thisproc
      end
    end
    src << "\n\n\n\n" 
  end
  
testinfo = [{:type => "assert", :unary => ""     , :msg => "To within "},
            {:type => "refute", :unary => ".NOT.", :msg => "Outside of"}]
            
typeinfo = [
  {:tag => "realsp"   , :type => "REAL"   , :kind => "Single", :fmt => 'es25.18'                    },
  {:tag => "realdp"   , :type => "REAL"   , :kind => "Double", :fmt => 'es25.18'                    },
  {:tag => "complexsp", :type => "COMPLEX", :kind => "Single", :fmt => '"(",es25.18,",",es25.18,")"'},
  {:tag => "complexdp", :type => "COMPLEX", :kind => "Double", :fmt => '"(",es25.18,",",es25.18,")"'}]

File.open("proclist.inc" ,'w') do |f|
  f << create_proclist(testinfo,typeinfo)
  f << create_genlist(testinfo,typeinfo)
end

testinfo.each do |a|
  File.open("#{a[:type]}.inc" ,'w') do |f|
    typeinfo.each do |b|
      f << create_scalar(a,b)
      f << create_rank1(a,b)
      f << create_rank2(a,b)
    end
  end
end


