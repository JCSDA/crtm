require 'define/base'
module FGenMod
  module Define
  
    class CheckRelease < FGenMod::Define::Base
    
      def generate
        name=procedure_name
        type="TYPE(#{config.namespace}#{config.struct_name}_type)"

        # Declaration and argument type definition format
        dfmt=string_format(["#{config.struct_name}","Message_Log"])
        afmt=string_format([type,"CHARACTER(*), OPTIONAL"])

        # Base nspaces indent
        nspaces=12
        
        # The function
        str=strip_output(<<-EOT)
          FUNCTION #{name}( &
                     #{dfmt%"#{config.struct_name}"}, &  ! Input
                     #{dfmt%"RCS_Id"}, &  ! Revision control
                     #{dfmt%"Message_Log"}) &  ! Error messaging
                   RESULT( Error_Status )
            ! Arguments
            #{afmt%type}, INTENT(IN)  :: #{config.struct_name}
            #{afmt%"CHARACTER(*), OPTIONAL"}, INTENT(OUT) :: RCS_Id
            #{afmt%"CHARACTER(*), OPTIONAL"}, INTENT(IN)  :: Message_Log
            ! Function result
            INTEGER :: Error_Status
            ! Local parameters
            CHARACTER(*), PARAMETER :: ROUTINE_NAME = '#{name}'
            ! Local variables
            CHARACTER(256) :: Message

            ! Set up
            Error_Status = SUCCESS
            IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

            ! Check release is not too old
            IF ( #{config.struct_name}%Release < #{config.struct_name.upcase}_RELEASE ) THEN
              WRITE( Message, '( "A #{config.struct_name} data update is needed. ", &
                                &"#{config.struct_name} release is ", i0, &
                                &". Valid release is ",i0,"." )' ) &
                              #{config.struct_name}%Release, #{config.struct_name.upcase}_RELEASE
              #{fail_return("TRIM(Message)",:nspaces=>nspaces+2,:lstrip=>true)}
            END IF

            ! Check release is not too new
            IF ( #{config.struct_name}%Release > #{config.struct_name.upcase}_RELEASE ) THEN
              WRITE( Message, '( "A #{config.struct_name} software update is needed. ", &
                                &"#{config.struct_name} release is ", i0, &
                                &". Valid release is ",i0,"." )' ) &
                              #{config.struct_name}%Release, #{config.struct_name.upcase}_RELEASE
              #{fail_return("TRIM(Message)",:nspaces=>nspaces+2,:lstrip=>true)}
            END IF

          END FUNCTION #{name}
        EOT
      end
    end
  end
end
