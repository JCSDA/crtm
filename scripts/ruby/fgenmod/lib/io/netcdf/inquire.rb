require 'io/netcdf/base'
module FGenMod
  module IO
    module NetCDF
  
      class Inquire < FGenMod::IO::NetCDF::Base
    
        # Method to generate the inquire procedure.
        # Most methods called here are in the 
        # IO::NetCDF::Base class and are shared with 
        # the Read and Write classes.
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
              
            END FUNCTION #{name}
          EOT
        end
  
        
        # --------------
        # helper methods
        # --------------
      end
    end
  end
end
