require 'fgenmod_defbase'
module FGenMod
  module Def
  
    class Assign < FGenMod::Def::Base
    
      def generate
        name=procedure_name("Assign")
        type="TYPE(#{config.namespace}#{config.struct_name}_type)"

        # Declaration and argument type definition format
        dfmt=string_format(["#{config.struct_name}_out","Message_Log"])
        afmt=string_format([type,"CHARACTER(*), OPTIONAL"])

        # Base nspaces indent is 12
        nspaces=12
        
        # The function
        str=strip_output(<<-EOT)
          FUNCTION #{name}( &
                     #{dfmt%"#{config.struct_name}_in"}, &  ! Input
                     #{dfmt%"#{config.struct_name}_out"}, &  ! Output
                     #{dfmt%"RCS_Id"}, &  ! Revision control
                     #{dfmt%"Message_Log"}) &  ! Error messaging
                   RESULT( Error_Status )
            ! Arguments
            #{afmt%type}, INTENT(IN)     :: #{config.struct_name}_in
            #{afmt%type}, INTENT(IN OUT) :: #{config.struct_name}_out
            #{afmt%"CHARACTER(*), OPTIONAL"}, INTENT(OUT)    :: RCS_Id
            #{afmt%"CHARACTER(*), OPTIONAL"}, INTENT(IN)     :: Message_Log
            ! Function result
            INTEGER :: Error_Status
            ! Local parameters
            CHARACTER(*), PARAMETER :: ROUTINE_NAME = '#{name}'
            ! Local variables
            INTEGER :: #{config.dimdecl.join(",")}

            ! Set up
            Error_Status = SUCCESS
            IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

            ! ALL *input* pointers must be associated
            #{struct_assoc_test("_in",:nspaces=>nspaces)}
            
            ! Allocate data arrays
            #{alloc_struct(:nspaces=>nspaces)}
            IF ( Error_Status /= SUCCESS ) THEN
              #{fail_return("'Error allocating output structure'",:nspaces=>nspaces+2,:lstrip=>true)}
            END IF
            
            ! Assign non-dimension scalar members
            #{scalar_assign(:nspaces=>nspaces)}
            
            ! Copy array data
            #{array_assign(:nspaces=>nspaces)}
            
          END FUNCTION #{name}
        EOT
      end
  
      
      # --------------
      # helper methods
      # --------------
      # Method to construct the output
      # structure allocation statement
      # in the assign function
      def alloc_struct(args={})
        nspaces = args[:nspaces] ? args[:nspaces] : 0
        # Build the pretty output format
        dfmt=string_format(config.dim_list)
        # Build the structure allocation call
        cmd="Error_Status = #{procedure_name("Allocate")}( "; n=cmd.length+nspaces
        str=""
        config.dim_list.each do |d|
          str<<indent(n)+"#{config.struct_name}_in%#{dfmt%d}, &\n"
        end
        cmd<<str.lstrip
        cmd<<indent(n)<<"#{config.struct_name}_out, &\n"
        cmd<<indent(n)<<"Message_Log=Message_Log )"
      end

      # Method to construct the scalar
      # component assignment statements
      # in the assign function
      def scalar_assign(args={})
        nspaces = args[:nspaces] ? args[:nspaces] : 0
        # Build the pretty output format
        sfmt=string_format(config.scalar_list.collect {|s| s[:name]})
        # Build the scalar assignment statements
        cmd=""
        config.scalar_list.each {|s| cmd<<indent(nspaces)<<"#{config.struct_name}_out%#{sfmt%s[:name]} = #{config.struct_name}_in%#{s[:name]}\n"}
        cmd.lstrip.chomp  # Strip leading spaces of first line, newline of last line
      end

      # Method to construct the array
      # component assignment statements
      # in the assign function
      def array_assign(args={})
        nspaces = args[:nspaces] ? args[:nspaces] : 0
        # Build the pretty output format
        afmt=string_format(config.array_list.collect {|a| a[:name]})
        cmd=""
        config.array_list.each {|a| cmd<<indent(nspaces)<<"#{config.struct_name}_out%#{afmt%a[:name]} = #{config.struct_name}_in%#{a[:name]}\n"}
        cmd.lstrip.chomp  # Strip leading spaces of first line, newline of last line
      end
    end
  end
end
