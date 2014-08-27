#! /usr/bin/env ruby

# Ruby script to generate the syntactic sugar procedures for
# the individual bitflag settings.

# The generated files should then be inserted in the correct place
# in the structure definition module.

CONFIG={:structname=>'oSRF',
        :indefinite_article=>'an',
        :is_prefix=>'Is_',
        :set_prefix=>'Set_',
        :clear_prefix=>'Clear_',
        :flags=>[{:name=>'Interpolated'         , :desc=>'being interpolated'},
                 {:name=>'Integrated'           , :desc=>'being integrated'},
                 {:name=>'F0_Computed'          , :desc=>'having the central frequency computed'},
                 {:name=>'Linearly_Interpolated', :desc=>'being linearly interpolated. Default is spline'},
                 {:name=>'Gaussian_Integrated'  , :desc=>'begin integrated via Gaussian quadrature. Default is Simpsons'},
                 {:name=>'Difference'           , :desc=>'being a difference'}]}


def parameterlist(config)
  # Find longest flagname
  maxlen = config[:flags].inject(0) do |memo, item|
    memo >= item[:name].length ? memo : item[:name].length
  end
  # Construct include file
  parameter=<<-EOF
  ! The bit positions for the various flags
  INTEGER, PUBLIC, PARAMETER :: BEGIN_FLAG_POSITION = 0
  INTEGER, PUBLIC, PARAMETER :: END_FLAG_POSITION   = BIT_SIZE(0)
  EOF
  config[:flags].each_with_index do |item, index|
    lenpad = maxlen - item[:name].length
    this_parameter=<<-EOF
  INTEGER, PUBLIC, PARAMETER :: #{item[:name].upcase}_FLAG#{' '*lenpad} = #{index}   
    EOF
    parameter << this_parameter
  end
  parameter
end

def uselist(config)
  prefix = [config[:is_prefix],config[:set_prefix],config[:clear_prefix]]
  list=<<-EOF
  
!--------------------------------------------------------------------------------
! USE list of the oSRF flag check, set, and clear procedures.
!--------------------------------------------------------------------------------
  USE oSRF_Define, ONLY: &
  EOF
  prefix.each do |p|
    config[:flags].each do |f|
      this_list=<<-EOF
    #{config[:structname]}_#{p}#{f[:name]}, &
      EOF
      list << this_list
    end
  end
  list << "    #{config[:structname]}_#{config[:clear_prefix]}Flags"
end


def publiclist(config)
  prefix = [config[:is_prefix],config[:set_prefix],config[:clear_prefix]]
  list=<<-EOF
  
!--------------------------------------------------------------------------------
! Visibilities of the oSRF flag check, set, and clear procedures.
!--------------------------------------------------------------------------------
  EOF
  prefix.each do |p|
    config[:flags].each do |f|
      this_list=<<-EOF
  PUBLIC :: #{config[:structname]}_#{p}#{f[:name]}
      EOF
      list << this_list
    end
  end
  list << "  PUBLIC :: #{config[:structname]}_#{config[:clear_prefix]}Flags"
end


def isflagset(config)
  procedures = ''
  config[:flags].each do |f|
    this_procedure=<<-EOF

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   #{config[:structname]}_#{config[:is_prefix]}#{f[:name]}
!
! PURPOSE:
!   Elemental function to test if the #{f[:name]} bit flag in #{config[:indefinite_article]}
!   #{config[:structname]} object is set.
!
! CALLING SEQUENCE:
!   is_set = #{config[:structname]}_#{config[:is_prefix]}#{f[:name]}( #{config[:structname]} )
!
! OBJECTS:
!   #{config[:structname]}: Object to be tested.
!   #{' '*config[:structname].length}  UNITS:      N/A
!   #{' '*config[:structname].length}  TYPE:       #{config[:structname]}_type
!   #{' '*config[:structname].length}  DIMENSION:  Scalar or any rank
!   #{' '*config[:structname].length}  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!   is_set: The return value is a logical value.
!            .TRUE.  - The flag is set.
!            .FALSE. - The flag is NOT set.
!           UNITS:      N/A
!           TYPE:       LOGICAL
!           DIMENSION:  Same as #{config[:structname]} input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION #{config[:structname]}_#{config[:is_prefix]}#{f[:name]}(#{config[:structname]}) RESULT(is_set)
    TYPE(#{config[:structname]}_type), INTENT(IN) :: #{config[:structname]}
    LOGICAL :: is_set
    is_set = .FALSE.
    IF ( .NOT. #{config[:structname]}_Associated(#{config[:structname]}) ) RETURN
    is_set = #{config[:structname]}_Is_Flag_Set(#{config[:structname]}, #{f[:name].upcase}_FLAG)
  END FUNCTION #{config[:structname]}_#{config[:is_prefix]}#{f[:name]}
  
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
!   #{config[:structname]}_#{config[:set_prefix]}#{f[:name]}
!
! PURPOSE:
!   Elemental subroutine to set the #{f[:name]} bit flag in #{config[:indefinite_article]} 
!   #{config[:structname]} object.
!
! CALLING SEQUENCE:
!   CALL #{config[:structname]}_#{config[:set_prefix]}#{f[:name]}( #{config[:structname]} )
!
! OBJECTS:
!   #{config[:structname]}: Object to be altered.
!   #{' '*config[:structname].length}  UNITS:      N/A
!   #{' '*config[:structname].length}  TYPE:       #{config[:structname]}_type
!   #{' '*config[:structname].length}  DIMENSION:  Scalar or any rank
!   #{' '*config[:structname].length}  ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE #{config[:structname]}_#{config[:set_prefix]}#{f[:name]}( #{config[:structname]} )
    TYPE(#{config[:structname]}_type), INTENT(IN OUT) :: #{config[:structname]}
    IF ( .NOT. #{config[:structname]}_Associated(#{config[:structname]}) ) RETURN
    CALL #{config[:structname]}_Set_Flag(#{config[:structname]}, #{f[:name].upcase}_FLAG)
  END SUBROUTINE #{config[:structname]}_#{config[:set_prefix]}#{f[:name]}
  
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
!   #{config[:structname]}_#{config[:clear_prefix]}#{f[:name]}
!
! PURPOSE:
!   Elemental subroutine to clear the #{f[:name]} bit flag in #{config[:indefinite_article]} 
!   #{config[:structname]} object.
!
! CALLING SEQUENCE:
!   CALL #{config[:structname]}_#{config[:clear_prefix]}#{f[:name]}( #{config[:structname]} )
!
! OBJECTS:
!   #{config[:structname]}: Object to be altered.
!   #{' '*config[:structname].length}  UNITS:      N/A
!   #{' '*config[:structname].length}  TYPE:       #{config[:structname]}_type
!   #{' '*config[:structname].length}  DIMENSION:  Scalar or any rank
!   #{' '*config[:structname].length}  ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE #{config[:structname]}_#{config[:clear_prefix]}#{f[:name]}( #{config[:structname]} )
    TYPE(#{config[:structname]}_type), INTENT(IN OUT) :: #{config[:structname]}
    IF ( .NOT. #{config[:structname]}_Associated(#{config[:structname]}) ) RETURN
    CALL #{config[:structname]}_Clear_Flag( #{config[:structname]}, #{f[:name].upcase}_FLAG )
  END SUBROUTINE #{config[:structname]}_#{config[:clear_prefix]}#{f[:name]}
  
    EOF
    procedures << this_procedure
  end
  procedures
end


def genericflag(config)
  procedures=<<-EOF

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   #{config[:structname]}_#{config[:clear_prefix]}Flags
!
! PURPOSE:
!   Elemental subroutine to clear ALL #{config[:structname]} flags.
!
! CALLING SEQUENCE:
!   CALL #{config[:structname]}_#{config[:clear_prefix]}Flags( #{config[:structname]} )
!
! OBJECTS:
!   #{config[:structname]}: Object to be altered.
!   #{' '*config[:structname].length}  UNITS:      N/A
!   #{' '*config[:structname].length}  TYPE:       #{config[:structname]}_type
!   #{' '*config[:structname].length}  DIMENSION:  Scalar or any rank
!   #{' '*config[:structname].length}  ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE #{config[:structname]}_#{config[:clear_prefix]}Flags( #{config[:structname]} )
    TYPE(#{config[:structname]}_type), INTENT(IN OUT) :: #{config[:structname]}
    INTEGER :: n
    DO n = BEGIN_FLAG_POSITION, END_FLAG_POSITION
      CALL #{config[:structname]}_#{config[:clear_prefix]}Flag( #{config[:structname]}, n )
    END DO
  END SUBROUTINE #{config[:structname]}_#{config[:clear_prefix]}Flags


  ! ---------------------
  ! Generic flag routines
  ! ---------------------

  ELEMENTAL FUNCTION #{config[:structname]}_#{config[:is_prefix]}Flag_Set(self, flag) RESULT(is_set)
    TYPE(#{config[:structname]}_type), INTENT(IN) :: self
    INTEGER#{' '*(config[:structname].length+4)}, INTENT(IN) :: flag
    LOGICAL :: is_set
    is_set = BTEST(self%Flags,flag)
  END FUNCTION #{config[:structname]}_#{config[:is_prefix]}Flag_Set


  ELEMENTAL SUBROUTINE #{config[:structname]}_#{config[:set_prefix]}Flag(self, flag)
    TYPE(#{config[:structname]}_type), INTENT(IN OUT) :: self
    INTEGER#{' '*(config[:structname].length+4)}, INTENT(IN)     :: flag
    self%Flags = IBSET(self%Flags,flag)
  END SUBROUTINE #{config[:structname]}_#{config[:set_prefix]}Flag


  ELEMENTAL SUBROUTINE #{config[:structname]}_#{config[:clear_prefix]}Flag(self, flag)
    TYPE(#{config[:structname]}_type), INTENT(IN OUT) :: self
    INTEGER#{' '*(config[:structname].length+4)}, INTENT(IN)     :: flag
    self%Flags = IBCLR(self%Flags,flag)
  END SUBROUTINE #{config[:structname]}_#{config[:clear_prefix]}Flag
  
  EOF
  procedures
end


File.open("#{CONFIG[:structname]}_Flag_Parameters.inc" ,'w') do |f|
  f << parameterlist(CONFIG)
end
File.open("#{CONFIG[:structname]}_Flag_Usage.inc" ,'w') do |f|
  f << uselist(CONFIG)
end
File.open("#{CONFIG[:structname]}_Flag_Visibilities.inc" ,'w') do |f|
  f << publiclist(CONFIG)
end
File.open("#{CONFIG[:structname]}_Flag_Procedures.inc" ,'w') do |f|
  f << isflagset(CONFIG)
  f << setflag(CONFIG)
  f << clearflag(CONFIG)
  f << genericflag(CONFIG)
end
