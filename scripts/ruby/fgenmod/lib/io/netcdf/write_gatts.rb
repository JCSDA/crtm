require 'io/netcdf/base'
module FGenMod
  module IO
    module NetCDF
  
      class Write_GAtts < FGenMod::IO::NetCDF::Base
    
        # Default indent
        NSPACES = 14
        
        # Method to generate the global attribute
        # write procedure.
        def generate

          # Declaration and argument type definition format
          gfmt = string_format(["Message_Log"]+(DEFAULT_GATTS + config.gatts).uniq)
          afmt = string_format(["CHARACTER(*), OPTIONAL"])

          # Construct the procedure
          str = strip_output(<<-EOT)
            FUNCTION #{class_name}( &
                       #{gfmt%"NC_Filename"}, &  ! Input
                       #{gfmt%"NC_FileID"}, &  ! Input
                       #{procedure_gatts_def(gfmt,:nspaces=>NSPACES+9)}
                       #{gfmt%"Message_Log"}) &  ! Error messaging
                     RESULT ( Error_Status )
              ! Arguments
              #{afmt%"CHARACTER(*)"}, INTENT(IN) :: NC_Filename
              #{afmt%"INTEGER"}, INTENT(IN) :: NC_FileID
              #{argument_gatts_def(afmt,:nspaces=>NSPACES)}
              #{afmt%"CHARACTER(*), OPTIONAL"}, INTENT(IN) :: Message_Log
              ! Function result
              INTEGER :: Error_Status
              ! Local parameters
              CHARACTER(*), PARAMETER :: ROUTINE_NAME = '#{class_name}'
              ! Local variables
              CHARACTER(256)  :: Message
              INTEGER :: NF90_Status

              ! Read the global attributes
              #{user_gatts_io(:nspaces=>NSPACES)}
              
            END FUNCTION #{class_name}
          EOT
        end
              
        # --------------
        # helper methods
        # --------------

      end
    end
  end
end
