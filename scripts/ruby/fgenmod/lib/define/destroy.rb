require 'define/base'
module FGenMod
  module Define
  
    class Destroy < FGenMod::Define::Base
    
      def generate
        name=procedure_name
        type="TYPE(#{config.namespace}#{config.struct_name}_type)"

        # Classes "called" in this procedure
        (clear=Clear.new).config=self.config
        (associated=Associated.new).config=self.config
        
        # Declaration and argument type definition format
        dfmt=string_format(["#{config.struct_name}","Message_Log"])
        afmt=string_format([type,"CHARACTER(*), OPTIONAL"])

        # The function
        str=strip_output(<<-EOT)
          FUNCTION #{name}( &
                     #{dfmt%"#{config.struct_name}"}, &  ! Output
                     #{dfmt%"No_Clear"}, &  ! Optional input
                     #{dfmt%"RCS_Id"}, &  ! Revision control
                     #{dfmt%"Message_Log"}) &  ! Error messaging
                   RESULT(Error_Status)
            ! Arguments
            #{afmt%type}, INTENT(IN OUT) :: #{config.struct_name}
            #{afmt%"INTEGER,      OPTIONAL"}, INTENT(IN)     :: No_Clear
            #{afmt%"CHARACTER(*), OPTIONAL"}, INTENT(OUT)    :: RCS_Id
            #{afmt%"CHARACTER(*), OPTIONAL"}, INTENT(IN)     :: Message_Log
            ! Function result
            INTEGER :: Error_Status
            ! Local parameters
            CHARACTER(*), PARAMETER :: ROUTINE_NAME = '#{name}'
            ! Local variables
            CHARACTER(256)  :: Message
            LOGICAL :: Clear
            INTEGER :: Allocate_Status
            
            ! Set up
            Error_Status = SUCCESS
            IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID
            
            ! Reset the dimension indicators
            #{dim_clear(:nspaces=>12)}
            
            ! Default is to clear scalar members...
            Clear = .TRUE.
            ! ....unless the No_Clear argument is set
            IF ( PRESENT( No_Clear ) ) THEN
              IF ( No_Clear == 1 ) Clear = .FALSE.
            END IF
            IF ( Clear ) CALL #{clear.procedure_name}(#{config.struct_name})
            
            ! If ALL pointer members are NOT associated, do nothing
            IF ( .NOT. #{associated.procedure_name}(#{config.struct_name}) ) RETURN
            
            ! Deallocate the pointer members
            #{deallocate(:nspaces=>12)}
            IF ( Allocate_Status /= 0 ) THEN
              WRITE( Message, '("Error deallocating #{config.struct_name}. STAT = ",i0)') &
                              Allocate_Status
              #{fail_return("TRIM(Message)",:nspaces=>14,:lstrip=>true)}
            END IF

            ! Decrement and test allocation counter
            #{config.struct_name}%n_Allocates = #{config.struct_name}%n_Allocates - 1
            IF ( #{config.struct_name}%n_Allocates /= 0 ) THEN
              WRITE( Message, '("Allocation counter /= 0, Value = ",i0)') &
                              #{config.struct_name}%n_Allocates
              #{fail_return("TRIM(Message)",:nspaces=>14,:lstrip=>true)}
            END IF
            
          END FUNCTION #{name}
        EOT
      end
      
      # --------------
      # helper methods
      # --------------
      # Method to construct the array
      # component deallocation statement
      # in the destroy function
      def deallocate(args={})
        nspaces = args[:nspaces] ? args[:nspaces] : 0
        cmd="DEALLOCATE( "; n=cmd.length+nspaces
        str=""
        config.array_list.each do |a|
          str<<indent(n)+"#{config.struct_name}%#{a[:name]}, &\n"
        end
        cmd<<str.lstrip<<indent(n)+"STAT = Allocate_Status )"
      end

      # Method to construct the dimension
      # reinitialisation statements in the
      # destroy function
      def dim_clear(args={})
        n = args[:nspaces] ? args[:nspaces] : 0
        # Build a formatted list
        dlist=list_format(config.dim_list)
        # Construct the dimension
        # reinitialisation statements
        cmd=""
        dlist.each {|d| cmd<<indent(n)+"#{config.struct_name}%#{d} = 0\n"}
        cmd.lstrip.chomp # Strip leading spaces of first line, newline of last line
      end
    end
  end
end
