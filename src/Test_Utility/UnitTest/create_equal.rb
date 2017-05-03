#! /usr/bin/env ruby

# Brain dead script to create the assert_equal and refute_equal
# procedures for scalar, rank-1, and rank-2 inputs

  def create_scalar(testinfo, typeinfo)
    src=<<-EOF
  SUBROUTINE #{typeinfo[:tag]}_#{testinfo[:type]}_equal_s( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    #{typeinfo[:type]}(#{typeinfo[:kind]}), INTENT(IN) :: Expected, Actual
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::#{testinfo[:type].capitalize}_Equal[#{typeinfo[:type]}(#{typeinfo[:kind]})]'
    ! Variables
    LOGICAL :: test
    LOGICAL :: verbose
    CHARACTER(SL) :: message
    ! Setup
    ! ...Assign the test
    test = #{testinfo[:unary]}(Expected #{typeinfo[:op]} Actual)
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
    WRITE( message, '(a,7x,"Expected: ",#{typeinfo[:fmt]},a,&
                       &7x,"And got:  ",#{typeinfo[:fmt]})') &
                    CRLF, Expected, CRLF, Actual
    ! Load the object with the message
    CALL Set_Property( &
      self, &
      Level     = TEST_LEVEL, &
      Procedure = PROCEDURE_NAME, &
      Message   = message )
    ! Output the result
    IF ( verbose ) CALL Display_Message( self )
  END SUBROUTINE #{typeinfo[:tag]}_#{testinfo[:type]}_equal_s
  
  
    EOF
    src
  end

  def create_rank1(testinfo, typeinfo)
    src=<<-EOF
  SUBROUTINE #{typeinfo[:tag]}_#{testinfo[:type]}_equal_r1( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    #{typeinfo[:type]}(#{typeinfo[:kind]}), INTENT(IN) :: Expected(:), Actual(:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::#{testinfo[:type].capitalize}_Equal[#{typeinfo[:type]}(#{typeinfo[:kind]})]'
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
      CALL self%#{testinfo[:type].capitalize}_Equal( Expected(i), Actual(i) )
    END DO
  END SUBROUTINE #{typeinfo[:tag]}_#{testinfo[:type]}_equal_r1
  
  
    EOF
    src
  end

  def create_rank2(testinfo, typeinfo)
    src=<<-EOF
  SUBROUTINE #{typeinfo[:tag]}_#{testinfo[:type]}_equal_r2( self, Expected, Actual )
    ! Arguments
    CLASS(UnitTest_type), INTENT(IN OUT) :: self
    #{typeinfo[:type]}(#{typeinfo[:kind]}), INTENT(IN) :: Expected(:,:), Actual(:,:)
    ! Parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::#{testinfo[:type].capitalize}_Equal[#{typeinfo[:type]}(#{typeinfo[:kind]})]'
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
        CALL self%#{testinfo[:type].capitalize}_Equal( Expected(i,j), Actual(i,j) )
      END DO
    END DO
  END SUBROUTINE #{typeinfo[:tag]}_#{testinfo[:type]}_equal_r2
  
  
    EOF
    src
  end

  def create_proclist(testinfo,typeinfo)
    src=""
    testinfo.each do |a|
      typeinfo.each do |b|
        thisproc=<<-EOF
    PROCEDURE, PASS(self) :: #{b[:tag]}_#{a[:type]}_equal_s
    PROCEDURE, PASS(self) :: #{b[:tag]}_#{a[:type]}_equal_r1
    PROCEDURE, PASS(self) :: #{b[:tag]}_#{a[:type]}_equal_r2
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
    GENERIC, PUBLIC :: #{a[:type].capitalize}_Equal => &
      EOF
      src << thistest
      typeinfo.each do |b|
        thisproc=<<-EOF
      #{b[:tag]}_#{a[:type]}_equal_s, #{b[:tag]}_#{a[:type]}_equal_r1, #{b[:tag]}_#{a[:type]}_equal_r2, &
        EOF
        src << thisproc
      end
    end
    src << "\n\n\n\n" 
  end
  
testinfo = [{:type => "assert", :unary => ""     },
            {:type => "refute", :unary => ".NOT."}]
            
typeinfo = [
  {:tag => "intbyte"  , :type => "INTEGER"     , :kind => "Byte"  , :op => "=="       , :fmt => 'i0'                         },
  {:tag => "intshort" , :type => "INTEGER"     , :kind => "Short" , :op => "=="       , :fmt => 'i0'                         },
  {:tag => "intlong"  , :type => "INTEGER"     , :kind => "Long"  , :op => "=="       , :fmt => 'i0'                         },
  {:tag => "realsp"   , :type => "REAL"        , :kind => "Single", :op => ".EqualTo.", :fmt => 'es25.18'                    },
  {:tag => "realdp"   , :type => "REAL"        , :kind => "Double", :op => ".EqualTo.", :fmt => 'es25.18'                    },
  {:tag => "complexsp", :type => "COMPLEX"     , :kind => "Single", :op => ".EqualTo.", :fmt => '"(",es25.18,",",es25.18,")"'},
  {:tag => "complexdp", :type => "COMPLEX"     , :kind => "Double", :op => ".EqualTo.", :fmt => '"(",es25.18,",",es25.18,")"'},
  {:tag => "char"     , :type => "CHARACTER(*)", :kind => ""      , :op => "=="       , :fmt => '">",a,"<"'                  }]

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


