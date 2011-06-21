#! /usr/bin/env ruby

# Ruby script to generate the syntactic sugar procedures for
# the individual bitflag settings.

# The generated files should then be inserted in the correct place
# in the structure definition module.

CONFIG={:structname=>'SpcCoeff',
        :flags=>[{:name=>'solar' , :desc=>'solar sensitive'},
                 {:name=>'zeeman', :desc=>'Zeeman affected'}]}


def publiclist(config)
  list = ''
  config[:flags].each do |f|
    this_list=<<-EOF
  PUBLIC :: #{config[:structname]}_Is#{f[:name].capitalize}, #{config[:structname]}_Set#{f[:name].capitalize}, #{config[:structname]}_Clear#{f[:name].capitalize}
    EOF
    list << this_list
  end
  list
end


def isflagset(config)
  procedures = ''
  config[:flags].each do |f|
    this_procedure=<<-EOF

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       #{config[:structname]}_Is#{f[:name].capitalize}
!
! PURPOSE:
!       Elemental function to test if #{config[:structname]} channels are flagged as being
!       #{f[:desc]}.
!
! CALLING SEQUENCE:
!       Status = #{config[:structname]}_Is#{f[:name].capitalize}( #{config[:structname]}, ChannelIndex=ChannelIndex )
!
! OBJECTS:
!       #{config[:structname]}:      Structure which is to be tested.
!                      UNITS:      N/A
!                      TYPE:       #{config[:structname]}_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       ChannelIndex:  Set this to the index corresponding to a particular
!                      channel in the #{config[:structname]} object to test if it is a
!                      #{f[:desc]} channel.
!                      If not specified, all the channels are tested.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Conformable with #{config[:structname]} input
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:        The return value is a logical value.
!                       .TRUE.  - The channel(s) is(are) #{f[:desc]}.
!                       .FALSE. - The channel(s) is(are) NOT #{f[:desc]}.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Same as #{config[:structname]} input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION #{config[:structname]}_Is#{f[:name].capitalize}(#{config[:structname]}, ChannelIndex) RESULT(Is_Set)
    TYPE(#{config[:structname]}_type), INTENT(IN) :: #{config[:structname]}
    INTEGER,   OPTIONAL, INTENT(IN) :: ChannelIndex
    LOGICAL :: Is_Set
    Is_Set = .FALSE.
    IF ( .NOT. #{config[:structname]}_Associated(#{config[:structname]}) ) RETURN
    Is_Set = #{config[:structname]}_IsFlagSet(#{config[:structname]}, #{f[:name].upcase}_FLAG, ChannelIndex=ChannelIndex)
  END FUNCTION #{config[:structname]}_Is#{f[:name].capitalize}
  
    EOF
    procedures << this_procedure
  end
  procedures
end


def setflag(config)
  procedures = ''
  config[:flags].each do |f|
    this_procedure=<<-EOF

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       #{config[:structname]}_Set#{f[:name].capitalize}
!
! PURPOSE:
!       Elemental subroutine to flag a #{config[:structname]} channel as #{f[:desc]}.
!
! CALLING SEQUENCE:
!       CALL #{config[:structname]}_Set#{f[:name].capitalize}( #{config[:structname]}, ChannelIndex=ChannelIndex )
!
! OBJECTS:
!       #{config[:structname]}:      Structure which is to be altered.
!                      UNITS:      N/A
!                      TYPE:       #{config[:structname]}_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUTS:
!       ChannelIndex:  Set this to the index corresponding to a particular
!                      channel in the #{config[:structname]} object to flag as a
!                      #{f[:desc]} channel.
!                      If not specified, all the channels are flagged.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Conformable with #{config[:structname]} input
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE #{config[:structname]}_Set#{f[:name].capitalize}( #{config[:structname]}, ChannelIndex )
    TYPE(#{config[:structname]}_type), INTENT(IN OUT) :: #{config[:structname]}
    INTEGER,   OPTIONAL, INTENT(IN)     :: ChannelIndex
    IF ( .NOT. #{config[:structname]}_Associated(#{config[:structname]}) ) RETURN
    CALL #{config[:structname]}_SetFlag(#{config[:structname]}, #{f[:name].upcase}_FLAG, ChannelIndex=ChannelIndex)
  END SUBROUTINE #{config[:structname]}_Set#{f[:name].capitalize}
  
    EOF
    procedures << this_procedure
  end
  procedures
end


def clearflag(config)
  procedures = ''
  config[:flags].each do |f|
    this_procedure=<<-EOF

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       #{config[:structname]}_Clear#{f[:name].capitalize}
!
! PURPOSE:
!       Elemental subroutine to flag a #{config[:structname]} channel as NOT being
!       #{f[:desc]}.
!
! CALLING SEQUENCE:
!       CALL #{config[:structname]}_Clear#{f[:name].capitalize}( #{config[:structname]}, ChannelIndex=ChannelIndex )
!
! OBJECTS:
!       #{config[:structname]}:      Structure which is to be altered.
!                      UNITS:      N/A
!                      TYPE:       #{config[:structname]}_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUTS:
!       ChannelIndex:  Set this to the index corresponding to a particular
!                      channel in the #{config[:structname]} object to indicate as being
!                      NOT #{f[:desc]}.
!                      If not specified, all the channels cleared.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Conformable with #{config[:structname]} input
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE #{config[:structname]}_Clear#{f[:name].capitalize}( #{config[:structname]}, ChannelIndex )
    TYPE(#{config[:structname]}_type), INTENT(IN OUT) :: #{config[:structname]}
    INTEGER,   OPTIONAL, INTENT(IN)     :: ChannelIndex
    IF ( .NOT. #{config[:structname]}_Associated(#{config[:structname]}) ) RETURN
    CALL #{config[:structname]}_ClearFlag( #{config[:structname]}, #{f[:name].upcase}_FLAG, ChannelIndex=ChannelIndex )
  END SUBROUTINE #{config[:structname]}_Clear#{f[:name].capitalize}
  
    EOF
    procedures << this_procedure
  end
  procedures
end


File.open('flag_procedures.include' ,'w') do |f|
  f << publiclist(CONFIG)
  f << "\n"
  f << isflagset(CONFIG)
  f << setflag(CONFIG)
  f << clearflag(CONFIG)
end
