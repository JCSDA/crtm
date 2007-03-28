require 'define/base'
module FGenMod
  module Define
  
    class Allocate < FGenMod::Define::Base
    
      def generate
        name=procedure_name
        type="TYPE(#{config.namespace}#{config.struct_name}_type)"

        # Classes "called" in this procedure
        (associated=Associated.new).config=self.config
        (destroy=Destroy.new).config=self.config
        
        # Declaration and argument type definition format
        dfmt=string_format(["#{config.struct_name}","Message_Log"]+config.dim_list)
        afmt=string_format([type,"CHARACTER(*), OPTIONAL"])

        # Base nspaces indent
        nspaces=12
        
        # The function
        str=strip_output(<<-EOT)
          FUNCTION #{name}( &
                     #{procedure_dim_def(dfmt,:nspaces=>nspaces+9)}
                     #{dfmt%"#{config.struct_name}"}, &  ! Output
                     #{dfmt%"RCS_Id"}, &  ! Revision control
                     #{dfmt%"Message_Log"}) &  ! Error messaging
                   RESULT( Error_Status )
            ! Arguments
            #{argument_dim_def(afmt,:nspaces=>nspaces)}
            #{afmt%type}, INTENT(IN OUT) :: #{config.struct_name}
            #{afmt%"CHARACTER(*), OPTIONAL"}, INTENT(OUT)    :: RCS_Id
            #{afmt%"CHARACTER(*), OPTIONAL"}, INTENT(IN)     :: Message_Log
            ! Function result
            INTEGER :: Error_Status
            ! Local parameters
            CHARACTER(*), PARAMETER :: ROUTINE_NAME = '#{name}'
            ! Local variables
            CHARACTER(256) :: Message
            INTEGER :: Allocate_Status
            
            ! Set up
            Error_Status = SUCCESS
            IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID
            
            ! Check dimensions
            #{dimension_size_test(:nspaces=>nspaces)}
            
            ! Check if ANY pointers are already associated.
            ! If they are, deallocate them but leave scalars.
            IF ( #{associated.procedure_name}( #{config.struct_name}, ANY_Test=1 ) ) THEN
              Error_Status = #{destroy.procedure_name}( &
                               #{config.struct_name}, &
                               No_Clear=1, &
                               Message_Log=Message_Log )
              IF ( Error_Status /= SUCCESS ) THEN
                #{fail_return("'Error deallocating #{config.struct_name} prior to allocation.'",:nspaces=>nspaces+4,:lstrip=>true)}
              END IF
            END IF
            
            ! Perform the pointer allocation
            #{array_allocate(:nspaces=>nspaces)}
            IF ( Allocate_Status /= 0 ) THEN
              WRITE(Message,'("Error allocating #{config.struct_name} data arrays. STAT = ",i0)') &
                            Allocate_Status
              #{fail_return("TRIM(Message)",:nspaces=>nspaces+2,:lstrip=>true)}
            END IF
            
            ! Assign the dimensions
            #{dim_assign(:nspaces=>nspaces)}
            
            ! Initialise the arrays
            #{array_init(:nspaces=>nspaces)}
            
            ! Increment and test the allocation counter
            #{config.struct_name}%n_Allocates = #{config.struct_name}%n_Allocates + 1
            IF ( #{config.struct_name}%n_Allocates /= 1 ) THEN
              WRITE( Message, '("Allocation counter /= 1, Value = ",i0)') &
                              #{config.struct_name}%n_Allocates
              #{fail_return("TRIM(Message)",:nspaces=>14,:lstrip=>true)}
            END IF
            
          END FUNCTION #{name}
        EOT
      end
      
      # --------------
      # helper methods
      # --------------
      # Method to construct the
      # allocate statement in the
      # allocate function
      def array_allocate(args={})
        nspaces = args[:nspaces] ? args[:nspaces] : 0
        cmd="ALLOCATE( "; n=cmd.length+nspaces
        str=""
        config.array_list.each do |a|
          dim_extent = a[:dims].collect {|d| d.join(":")}.join(",")
          str<<indent(n)<<"#{config.struct_name}%#{a[:name]}(#{dim_extent}), &\n"
        end
        cmd<<str.lstrip<<indent(n)<<"STAT = Allocate_Status )"
      end

      # Method to construct the dimension
      # assignment statements in the
      # allocate function
      def dim_assign(args={})
        nspaces = args[:nspaces] ? args[:nspaces] : 0
        # Build a formatted list
        dfmt=string_format(config.dim_list)
        dim_list=config.dim_list.collect {|d| "#{dfmt%d}"}
        # Build the assignment statements
        cmd=""
        dim_list.each {|d| cmd<<indent(nspaces)<<"#{config.struct_name}%#{dfmt%d} = #{d}\n"}
        cmd.lstrip.chomp  # Strip leading spaces of first line, newline of last line
      end

      # Method to construct the array
      # initialisation statemnts
      # in the allocate function
      def array_init(args={})
        nspaces = args[:nspaces] ? args[:nspaces] : 0
        # Get the pretty output format
        afmt=string_format(config.array_list.collect {|a| a[:name]})
        # Build all the initialisations
        cmd=""
        config.array_list.each do |a|
          # Only intrinsic types are initialisaed
          next if a[:type] == "TYPE"
          # Construct the initialistions
          cmd<<indent(nspaces)<<"#{config.struct_name}%#{afmt%a[:name]} = " 
          cmd<< case a[:type]
                  when "INTEGER"   then "0\n"
                  when "REAL"      then "ZERO\n"
                  when "COMPLEX"   then "CMPLX(ZERO,ZERO)\n"
                  when "CHARACTER" then "' '\n"
                  when "LOGICAL"   then ".FALSE.\n"
                  else raise StandardError, "*** Type #{a[:type]} array assign not yet implemented ***"
                end
        end
        cmd.lstrip.chomp  # strip leading spaces of first line, newline of last line
      end
    end
  end
end
