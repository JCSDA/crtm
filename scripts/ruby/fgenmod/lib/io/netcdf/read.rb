require 'io/netcdf/base'
module FGenMod
  module IO
    module NetCDF
  
      class Read < FGenMod::IO::NetCDF::Base
    
        # Method to generate the read procedure.
        # Most methods called here are in the 
        # IO::NetCDF::Base class and are shared with 
        # the Inquire and Write classes.
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

            END FUNCTION #{name}
          EOT
        end
      end
    end
  end
end
