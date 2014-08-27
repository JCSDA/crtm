#! /usr/bin/env ruby

# Ruby script to generate the syntactic sugar procedures for
# the individual sensor settings.

# The routines should eventually be placed in the SensorInfo_Define
# module for generic use.

# The generated files should then be inserted in the correct place
# in the structure definition module.

CONFIG={:structname=>'oSRF',
        :is_prefix=>'Is_',
        :set_prefix=>'Set_',
        :clear_prefix=>'Clear_',
        :flags=>[{:name=>'Microwave'  , :indefinite_article=>'a'},
                 {:name=>'Infrared'   , :indefinite_article=>'an'},
                 {:name=>'Visible'    , :indefinite_article=>'a'},
                 {:name=>'Ultraviolet', :indefinite_article=>'an'}]}


def uselist(config)
  prefix = [config[:is_prefix],config[:set_prefix]]
  list=<<-EOF
  
!--------------------------------------------------------------------------------
! USE list of the oSRF sensor check, set, and clear procedures.
!--------------------------------------------------------------------------------
  USE oSRF_Define, ONLY: &
  EOF
  prefix.each do |p|
    config[:flags].each do |f|
      this_list=<<-EOF
    #{config[:structname]}_#{p}#{f[:name]}_Sensor, &
      EOF
      list << this_list
    end
  end
  list << "    #{config[:structname]}_#{config[:clear_prefix]}Sensor"
end


def publiclist(config)
  prefix = [config[:is_prefix],config[:set_prefix]]
  list=<<-EOF
  
!--------------------------------------------------------------------------------
! Visibilities of the oSRF sensor check, set, and clear procedures.
!--------------------------------------------------------------------------------
  EOF
  prefix.each do |p|
    config[:flags].each do |f|
      this_list=<<-EOF
  PUBLIC :: #{config[:structname]}_#{p}#{f[:name]}_Sensor
      EOF
      list << this_list
    end
  end
  list << "  PUBLIC :: #{config[:structname]}_#{config[:clear_prefix]}Sensor"
end


def issensor(config)
  procedures = ''
  config[:flags].each do |f|
    this_procedure=<<-EOF

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   #{config[:structname]}_#{config[:is_prefix]}#{f[:name]}_Sensor
!
! PURPOSE:
!   Elemental function to test if the sensor type in the #{config[:structname]}
!   object is for #{f[:indefinite_article]} #{f[:name].downcase} instrument.
!
! CALLING SEQUENCE:
!   is_set = #{config[:structname]}_#{config[:is_prefix]}#{f[:name]}_Sensor( #{config[:structname]} )
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
!            .TRUE.  - The sensor is #{f[:indefinite_article]} #{f[:name].downcase} instrument.
!            .FALSE. - The sensor is NOT #{f[:indefinite_article]} #{f[:name].downcase} instrument.
!           UNITS:      N/A
!           TYPE:       LOGICAL
!           DIMENSION:  Same as #{config[:structname]} input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION #{config[:structname]}_#{config[:is_prefix]}#{f[:name]}_Sensor(#{config[:structname]}) RESULT(is_set)
    TYPE(#{config[:structname]}_type), INTENT(IN) :: #{config[:structname]}
    LOGICAL :: is_set
    is_set = #{config[:structname]}_#{config[:is_prefix]}Sensor(#{config[:structname]}, #{f[:name].upcase}_SENSOR)
  END FUNCTION #{config[:structname]}_#{config[:is_prefix]}#{f[:name]}_Sensor
  
    EOF
    procedures << this_procedure
  end
  procedures
end


def setsensor(config)
  procedures = ''
  config[:flags].each do |f|
    this_procedure=<<-EOF

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   #{config[:structname]}_#{config[:set_prefix]}#{f[:name]}_Sensor
!
! PURPOSE:
!   Elemental subroutine to set the sensor type in the #{config[:structname]}
!   object for #{f[:indefinite_article]} #{f[:name].downcase} instrument.
!
! CALLING SEQUENCE:
!   CALL #{config[:structname]}_#{config[:set_prefix]}#{f[:name]}_Sensor( #{config[:structname]} )
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

  ELEMENTAL SUBROUTINE #{config[:structname]}_#{config[:set_prefix]}#{f[:name]}_Sensor( #{config[:structname]} )
    TYPE(#{config[:structname]}_type), INTENT(IN OUT) :: #{config[:structname]}
    CALL #{config[:structname]}_#{config[:set_prefix]}Sensor(#{config[:structname]}, #{f[:name].upcase}_SENSOR)
  END SUBROUTINE #{config[:structname]}_#{config[:set_prefix]}#{f[:name]}_Sensor
  
    EOF
    procedures << this_procedure
  end
  procedures
end


def genericsensor(config)
  procedures=<<-EOF
  
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   #{config[:structname]}_#{config[:clear_prefix]}Sensor
!
! PURPOSE:
!   Elemental subroutine to reinitialise the sensor type in the #{config[:structname]}
!   object.
!
! CALLING SEQUENCE:
!   CALL #{config[:structname]}_#{config[:clear_prefix]}Sensor( #{config[:structname]} )
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

  ELEMENTAL SUBROUTINE #{config[:structname]}_#{config[:clear_prefix]}Sensor(oSRF)
    TYPE(#{config[:structname]}_type), INTENT(IN OUT) :: #{config[:structname]}
    #{config[:structname]}%Sensor_Type = INVALID_SENSOR
  END SUBROUTINE #{config[:structname]}_#{config[:clear_prefix]}Sensor


  ! --------------------------------
  ! Generic, private sensor routines
  ! --------------------------------

  ELEMENTAL FUNCTION #{config[:structname]}_#{config[:is_prefix]}Sensor(#{config[:structname]}, Sensor_Type) RESULT(is_set)
    TYPE(#{config[:structname]}_type), INTENT(IN) :: #{config[:structname]}
    INTEGER#{' '*(config[:structname].length+4)}, INTENT(IN) :: Sensor_Type
    LOGICAL :: is_set
    is_set = (#{config[:structname]}%Sensor_Type == Sensor_Type)
  END FUNCTION #{config[:structname]}_#{config[:is_prefix]}Sensor


  ELEMENTAL SUBROUTINE #{config[:structname]}_#{config[:set_prefix]}Sensor(#{config[:structname]}, Sensor_Type)
    TYPE(#{config[:structname]}_type), INTENT(IN OUT) :: #{config[:structname]}
    INTEGER#{' '*(config[:structname].length+4)}, INTENT(IN)     :: Sensor_Type
    #{config[:structname]}%Sensor_Type = Sensor_Type
  END SUBROUTINE #{config[:structname]}_#{config[:set_prefix]}Sensor

    EOF
  procedures
end


File.open("#{CONFIG[:structname]}_Sensor_Usage.inc" ,'w') do |f|
  f << uselist(CONFIG)
end
File.open("#{CONFIG[:structname]}_Sensor_Visibilities.inc" ,'w') do |f|
  f << publiclist(CONFIG)
end
File.open("#{CONFIG[:structname]}_Sensor_Procedures.inc" ,'w') do |f|
  f << issensor(CONFIG)
  f << setsensor(CONFIG)
  f << genericsensor(CONFIG)
end
