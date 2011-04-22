#! /usr/bin/env ruby

# Ruby script to generate the syntactic sugar procedures for
# the individual sensor settings[:name].

# The generated files should then be inserted in the correct place
# in the structure definition module.

CONFIG={:structname=>'SpcCoeff',
        :sensors=>[{:name=>'microwave'  , :pre=>'a' },
                   {:name=>'infrared'   , :pre=>'an'},
                   {:name=>'visible'    , :pre=>'a' },
                   {:name=>'ultraviolet', :pre=>'an'}]}


def publiclist(config)
  list = ''
  config[:sensors].each do |s|
    this_list=<<-EOF
  PUBLIC :: #{config[:structname]}_Is#{s[:name].capitalize}Sensor, #{config[:structname]}_Set#{s[:name].capitalize}Sensor
    EOF
    list << this_list
  end
  list
end


def issensor(config)
  procedures = ''
  config[:sensors].each do |s|
    this_procedure=<<-EOF

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       #{config[:structname]}_Is#{s[:name].capitalize}Sensor
!
! PURPOSE:
!       Elemental function to test if the #{config[:structname]} object is for
!       #{s[:pre]} #{s[:name]} sensor.
!
! CALLING SEQUENCE:
!       Status = #{config[:structname]}_Is#{s[:name].capitalize}Sensor( #{config[:structname]} )
!
! OBJECTS:
!       #{config[:structname]}:      Structure which is to be tested.
!                      UNITS:      N/A
!                      TYPE:       #{config[:structname]}_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:        The return value is a logical value.
!                       .TRUE.  - The sensor is #{s[:pre]} #{s[:name]} instrument.
!                       .FALSE. - The sensor is NOT #{s[:pre]} #{s[:name]} instrument.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Same as #{config[:structname]} input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION #{config[:structname]}_Is#{s[:name].capitalize}Sensor(#{config[:structname]}) RESULT(Is_Set)
    TYPE(#{config[:structname]}_type), INTENT(IN) :: #{config[:structname]}
    LOGICAL :: Is_Set
    Is_Set = #{config[:structname]}_IsSensor(#{config[:structname]}, #{s[:name].upcase}_SENSOR)
  END FUNCTION #{config[:structname]}_Is#{s[:name].capitalize}Sensor
  
    EOF
    procedures << this_procedure
  end
  procedures
end


def setsensor(config)
  procedures = ''
  config[:sensors].each do |s|
    this_procedure=<<-EOF

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       #{config[:structname]}_Set#{s[:name].capitalize}Sensor
!
! PURPOSE:
!       Elemental subroutine to set a #{config[:structname]} object as being
!       for #{s[:pre]} #{s[:name]} sensor.
!
! CALLING SEQUENCE:
!       CALL #{config[:structname]}_Set#{s[:name].capitalize}Sensor( #{config[:structname]} )
!
! OBJECTS:
!       #{config[:structname]}:      Structure which is to be altered.
!                      UNITS:      N/A
!                      TYPE:       #{config[:structname]}_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE #{config[:structname]}_Set#{s[:name].capitalize}Sensor( #{config[:structname]} )
    TYPE(#{config[:structname]}_type), INTENT(IN OUT) :: #{config[:structname]}
    CALL #{config[:structname]}_SetSensor(#{config[:structname]}, #{s[:name].upcase}_SENSOR)
  END SUBROUTINE #{config[:structname]}_Set#{s[:name].capitalize}Sensor
  
    EOF
    procedures << this_procedure
  end
  procedures
end


File.open('sensor_procedures.include' ,'w') do |f|
  f << publiclist(CONFIG)
  f << "\n"
  f << issensor(CONFIG)
  f << setsensor(CONFIG)
end
