require 'fgenmod_defbase'
module FGenMod
  module Def
  
    class Assoc < FGenMod::Def::Base
    
      def generate
        name=procedure_name("Associated")
        type="TYPE(#{config.namespace}#{config.struct_name}_type)"

        # Declaration and argument type definition format
        dfmt=string_format(["#{config.struct_name}","ANY_Test"])
        afmt=string_format([type,"INTEGER, OPTIONAL"])

        # The function
        str=strip_output(<<-EOT)
          FUNCTION #{name}( &
                     #{dfmt%"#{config.struct_name}"}, &  ! Input
                     #{dfmt%"ANY_Test"}) &  ! Optional input
                   RESULT(Association_Status)
            ! Arguments
            #{afmt%type}, INTENT(IN) :: #{config.struct_name}
            #{afmt%"INTEGER, OPTIONAL"}, INTENT(IN) :: ANY_Test
            ! Function result
            LOGICAL :: Association_Status
            ! Local variables
            LOGICAL :: ALL_Test
            
            ! Default is to test ALL the pointer members
            ! for a true association status....
            ALL_Test = .TRUE.
            ! ...unless the ANY_Test argument is set.
            IF ( PRESENT( ANY_Test ) ) THEN
              IF ( ANY_Test == 1 ) ALL_Test = .FALSE.
            END IF
            
            ! Test the structure associations    
            Association_Status = .FALSE.
            IF (ALL_Test) THEN
              #{assoc_test("AND",:nspaces=>14)}
            ELSE
              #{assoc_test("OR",:nspaces=>14)}
            END IF
            
          END FUNCTION #{name}
        EOT
      end
  
      # --------------
      # helper methods
      # --------------
      # Method to generate the IF statements
      # in the associated function
      def assoc_test(operator,args={})
        nspaces = args[:nspaces] ? args[:nspaces] : 0
        # Build a formatted list
        afmt=string_format(config.array_list.collect {|a| a[:name]})
        array_list=config.array_list.collect do |a|
          "#{config.struct_name}%#{afmt%a[:name]}"
        end
        # Build the IF test
        cmd="IF ("; n=cmd.length+nspaces
        if array_list.length>1
          str=""
          array_list[0..-2].each do |a|
            str<<indent(n)<<"ASSOCIATED(#{a}) .#{operator}. &\n"
          end
          cmd<<str.lstrip<<indent(n)<<"ASSOCIATED(#{array_list.last})) THEN\n"
        else
          cmd<<"ASSOCIATED(#{array_list.first})) THEN\n"  
        end
        cmd<<indent(n-2)<<"Association_Status = .TRUE.\n"
        cmd<<indent(n-4)<<"END IF"
      end
    end
  end
end
