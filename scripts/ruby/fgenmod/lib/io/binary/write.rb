require 'io/binary/base'
module FGenMod
  module IO
    module Binary
  
      class Write < FGenMod::IO::Binary::Base
    
        # Method to generate the write procedure.
        # Most methods called here are in the 
        # IO::Binary::Base class and are shared
        # with the Read and Inquire classes.
        def generate
          name = self.procedure_name
          type = "TYPE(#{config.namespace}#{config.struct_name}_type)"

          # Declaration and argument type definition format
          dfmt = string_format([config.struct_name,"Message_Log"])
          afmt = string_format([type,"CHARACTER(*), OPTIONAL"])

          # Base nspaces indent
          nspaces = 14
          
          # Construct the procedure
          str = strip_output(<<-EOT)
            FUNCTION #{name}( &
                       #{dfmt%"Filename"}, &  ! Input
                       #{dfmt%config.struct_name}, &  ! Output
                       #{dfmt%"Quiet"}, &  ! Optional Input
                       #{dfmt%"RCS_Id"}, &  ! Revision Control
                       #{dfmt%"Message_Log"}) &  ! Error Messaging
                     RESULT( Error_Status )
              ! Arguments
              #{afmt%"CHARACTER(*)"}, INTENT(IN)  :: Filename
              #{afmt%type}, INTENT(IN)  :: #{config.struct_name}
              #{afmt%"INTEGER     , OPTIONAL"}, INTENT(IN)  :: Quiet
              #{afmt%"CHARACTER(*), OPTIONAL"}, INTENT(OUT) :: RCS_Id
              #{afmt%"CHARACTER(*), OPTIONAL"}, INTENT(IN)  :: Message_Log
              ! Function result
              INTEGER :: Error_Status
              ! Local parameters
              CHARACTER(*), PARAMETER :: ROUTINE_NAME = '#{name}'
              CHARACTER(*), PARAMETER :: FILE_STATUS_ON_ERROR = 'DELETE'
              ! Local variables
              CHARACTER(256) :: Message
              LOGICAL :: Noisy
              INTEGER :: IO_Status
              INTEGER :: FileID
              INTEGER :: Scalar_IO_Status(#{config.scalar_list.length})
              INTEGER :: Array_IO_Status(#{config.array_list.length})
              INTEGER :: i

              ! Set up
              Error_Status = SUCCESS
              IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

              ! Output informational messages...
              Noisy = .TRUE.
              ! ...unless the Quiet keyword is set
              IF ( PRESENT(Quiet) ) THEN
                IF ( Quiet == SET ) Noisy = .FALSE.
              END IF

              ! Check structure association status
              #{struct_assoc_test("",:nspaces=>nspaces)}
              
              ! Check the release
              #{check_release(:nspaces=>nspaces)}

              ! Check the dimensions
              #{dimension_size_test(:prefix=>true,:nspaces=>nspaces)}

              ! Open the Binary format #{config.struct_name} file
              #{open_binary_file(:nspaces=>nspaces)}

              ! Write the Release and Version information
              #{release_version_io(:nspaces=>nspaces)}
              
              ! Write the data dimensions
              #{dimension_io(:nspaces=>nspaces)}

              ! Write the scalar data items
              #{component_io("scalar",:nspaces=>nspaces)}

              ! Write the array data items
              #{component_io("array",:nspaces=>nspaces)}

              ! Close the file
              #{close_binary_file(:nspaces=>nspaces)}
              
              ! Output an info message
              #{info_message(:nspaces=>nspaces)}

            END FUNCTION #{name}
          EOT
        end
      end
    end
  end
end
