require 'io/binary/base'
module FGenMod
  module IO
    module Binary
  
      class Inquire < FGenMod::IO::Binary::Base
    
        # Method to generate the inquire procedure.
        # Most methods called here are in the 
        # IO::Binary::Base class and are shared
        # with the Read and Write classes.
        def generate
          name=procedure_name
          type="TYPE(#{config.namespace}#{config.struct_name}_type)"

          # Declaration and argument type definition format
          tfmt=string_format(["INTEGER","CHARACTER(*)"])
          dfmt=string_format(["Message_Log"]+config.dim_list)
          afmt=string_format(["CHARACTER(*), OPTIONAL"])

          # Base nspaces indent
          nspaces=14
          
          # The function
          str=strip_output(<<-EOT)
            FUNCTION #{name}( &
                       #{dfmt%"Filename"}, &  ! Input
                       #{procedure_dim_def(dfmt,:out=>true,:optional=>true,:nspaces=>nspaces+9)}
                       #{dfmt%"Release"}, &  ! Optional Output
                       #{dfmt%"Version"}, &  ! Optional Output
                       #{dfmt%"RCS_Id"}, &  ! Revision Control
                       #{dfmt%"Message_Log"}) &  ! Error Messaging
                     RESULT( Error_Status )
              ! Arguments
              #{afmt%"CHARACTER(*)"}, INTENT(IN)  :: Filename
              #{argument_dim_def(tfmt,:out=>true,:optional=>true,:nspaces=>nspaces)}
              #{afmt%"INTEGER     , OPTIONAL"}, INTENT(OUT) :: Release
              #{afmt%"INTEGER     , OPTIONAL"}, INTENT(OUT) :: Version
              #{afmt%"CHARACTER(*), OPTIONAL"}, INTENT(OUT) :: RCS_Id
              #{afmt%"CHARACTER(*), OPTIONAL"}, INTENT(IN)  :: Message_Log
              ! Function result
              INTEGER :: Error_Status
              ! Local parameters
              CHARACTER(*), PARAMETER :: ROUTINE_NAME = '#{name}'
              ! Local variables
              CHARACTER(256) :: Message
              INTEGER :: IO_Status
              INTEGER :: FileID
              INTEGER :: Rel, Ver
              INTEGER :: #{config.dimdecl.join(",")}

              ! Set up
              Error_Status = SUCCESS
              IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

              ! Open the Binary format #{config.struct_name} file
              #{open_binary_file(:nspaces=>nspaces)}

              ! Read the Release and Version information
              #{release_version_io(:nspaces=>nspaces)}

              ! Read the data dimensions
              #{dimension_io(:nspaces=>nspaces)}

              ! Assign the return arguments
              !
              ! Dimensions
              #{optional_return_assign(config.dim_list,config.dimdecl,:nspaces=>nspaces)}
              ! Release/Version information
              #{optional_return_assign(%w{Release Version},%w{Rel Ver},:nspaces=>nspaces)}

              ! Close the file
              #{close_binary_file(:nspaces=>nspaces)}
              
            END FUNCTION #{name}
          EOT
        end
  
        
        # --------------
        # helper methods
        # --------------
        # Method to construct the Binary IO
        # inquiry assignment statements for
        # the optional arguments
        def optional_return_assign(output_list,local_list,args={})
          nspaces = args[:nspaces] ? args[:nspaces] : 0
          
          # Construct the statements
          cmd=""
          output_list.each_index do |i|
            cmd << indent(nspaces)<<"IF ( PRESENT(#{output_list[i]}) ) THEN\n"
            cmd << indent(nspaces)<<"  #{output_list[i]} = #{local_list[i]}\n"
            cmd << indent(nspaces)<<"END IF\n"
          end
          cmd.lstrip.chomp
        end
      end
    end
  end
end
