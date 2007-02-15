require 'io/netcdf/base'
module FGenMod
  module IO
    module NetCDF
  
      class Create < FGenMod::IO::NetCDF::Base
    
        # Default indent
        NSPACES = 14
        
        # Method to generate the netCDF
        # file create procedure.
        def generate
          name = "#{class_name}_#{module_name}"

          # Declaration and argument type definition format
          dfmt = string_format(["Message_Log"]+
                               config.dim_list+
                               (DEFAULT_GATTS + config.gatts).uniq)
          afmt = string_format(["CHARACTER(*), OPTIONAL"])

          # Construct the procedure
          str = strip_output(<<-EOT)
            FUNCTION #{name}( &
                       #{dfmt%"NC_Filename"}, &  ! Input
                       #{procedure_dim_def(dfmt,:nspaces=>NSPACES+9)}
                       #{dfmt%"NC_FileID"}, &  ! Output
                       #{procedure_gatts_def(dfmt,:nspaces=>NSPACES+9)}
                       #{dfmt%"Message_Log"}) &  ! Error messaging
                     RESULT ( Error_Status )
              ! Arguments
              #{afmt%"CHARACTER(*)"}, INTENT(IN) :: NC_Filename
              #{afmt%"INTEGER"}, INTENT(IN) :: NC_FileID
              #{argument_gatts_def(afmt,:nspaces=>NSPACES)}
              #{afmt%"CHARACTER(*), OPTIONAL"}, INTENT(IN) :: Message_Log
              ! Function result
              INTEGER :: Error_Status
              ! Local parameters
              CHARACTER(*), PARAMETER :: ROUTINE_NAME = '#{name}'
              ! Local variables
              CHARACTER(256) :: Message
              CHARACTER(10000) :: Long_String
              INTEGER :: NF90_Status

              ! Set up
              Error_Status = SUCCESS
              
              ! Check dimensions
              #{dimension_size_test(:nspaces=>NSPACES)}
              
              ! Create the data file
              NF90_Status = NF90_CREATE( NC_Filename,  &
                                         NF90_CLOBBER, &
                                         NC_FileID     )
              IF ( NF90_Status /= NF90_NOERR ) THEN
                Message = 'Error creating '//TRIM(NC_Filename)//' - '// &
                          TRIM(NF90_STRERROR(NF90_Status))
                #{fail_return("TRIM(Message)",:nspaces=>NSPACES+2,:lstrip=>true)}
              END IF

              ! Define the dimensions
              #{dimension_define}
              
              ! Write the global attributes
              #{call_gatts_io(:nspaces=>NSPACES)}

              ! Define the variables
              #{scalar_variable_define}
              
              
            END FUNCTION #{name}
          EOT
        end
              
        # --------------
        # helper methods
        # --------------
        # Method to construct the netCDF
        # dimension definition calls
        def dimension_define(args={})
          nspaces = args[:nspaces]  ? args[:nspaces] : NSPACES
          
          # Indent for this method
          n = 14
          
          # Construct the definition commands
          cmd = ""
          config.dim_list.each do |d|
            str = <<-EOT
              NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                          TRIM(#{d.upcase}_DIMNAME), &
                                          #{d}, &
                                          #{d}_DimID )
              IF ( NF90_Status /= NF90_NOERR ) THEN
                NF90_Status = NF90_CLOSE(NC_FileID)
                Message = 'Error defining the '//TRIM(#{d.upcase}_DIMNAME)//' dimension in '//&
                          TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR(NF90_Status))
                #{fail_return("TRIM(Message)",:nspaces=>n+2,:lstrip=>true)}
              END IF
              
            EOT
            cmd << strip_output(str,indent(nspaces))
          end
          cmd.lstrip.chomp
        end

        # Method to construct the
        # netCDF variable definition
        # and attribute calls for 
        # the scalar variables
        def scalar_variable_define(args={})
          nspaces = args[:nspaces]  ? args[:nspaces] : NSPACES
          
          # Indent for this method
          n = 14
          
          # Construct the definition commands
          cmd = ""
          config.scalar_list.each do |s|
            var = s[:name]
            str = <<-EOT
              !
              ! Define the #{var} variable
              NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                          #{var.upcase}_VARNAME, &
                                          #{var.upcase}_TYPE, &
                                          varid = varID )
              IF ( NF90_Status /= NF90_NOERR ) THEN
                NF90_Status = NF90_CLOSE(NC_FileID)
                Message = 'Error defining '//#{var.upcase}_VARNAME//' variable in '// &
                          TRIM(NC_Filename)//' - '// &
                          TRIM(NF90_STRERROR(NF90_Status))
                #{fail_return("TRIM(Message)",:nspaces=>n+2,:lstrip=>true)}
              END IF
              #{vatts_io(var,:nspaces=>n)}
            EOT
            cmd << strip_output(str,indent(nspaces))
          end
          cmd.lstrip.chomp
        end

        # Method to construct the
        # netCDF variable definition
        # and attribute calls for 
        # the array variables
        def array_variable_define(args={})
          nspaces = args[:nspaces]  ? args[:nspaces] : NSPACES
          
          # Indent for this method
          n = 14
          
          # Construct the definition commands
          cmd = ""
          config.array_list.each do |a|
            var = a[:name]
            str = <<-EOT
              !
              ! Define the #{var} variable
              NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                          #{var.upcase}_VARNAME, &
                                          #{var.upcase}_TYPE, &
                                          ############# 
                                          varid = varID )
              IF ( NF90_Status /= NF90_NOERR ) THEN
                NF90_Status = NF90_CLOSE(NC_FileID)
                Message = 'Error defining '//#{var.upcase}_VARNAME//' variable in '// &
                          TRIM(NC_Filename)//' - '// &
                          TRIM(NF90_STRERROR(NF90_Status))
                #{fail_return("TRIM(Message)",:nspaces=>n+2,:lstrip=>true)}
              END IF
              #{vatts_io(var,:nspaces=>n)}
            EOT
            cmd << strip_output(str,indent(nspaces))
          end
          cmd.lstrip.chomp
        end


        # Method to construct the variable
        # attribute write calls
        def vatts_io(var,args={})
          nspaces = args[:nspaces] ? args[:nspaces] : NSPACES

          # Indent for this method
          n = 14

          # Construct the attribute calls
          cmd = ""          
          DEFAULT_VATTS.each do |v|
            attname  = "#{v.upcase}_ATTNAME"
            attvalue = "#{var.upcase}_#{v.upcase}"
            varname  = "#{var.upcase}_VARNAME"
            str = <<-EOT
              NF90_Status = NF90_#{nf_action}_ATT( NC_FileID, &
                                          VarId, &
                                          #{attname}, &
                                          #{attvalue} )
              IF ( NF90_Status /= NF90_NOERR ) THEN
                NF90_Status = NF90_CLOSE(NC_FileID)
                Message = 'Error #{io_ing} '//#{varname}//' attribute #{io_tofrom} '//&
                          TRIM(NC_Filename)//' - '// &
                          TRIM(NF90_STRERROR(NF90_Status))
                #{fail_return("TRIM(Message)",:nspaces=>n+2,:lstrip=>true)}
              END IF
            EOT
            cmd << strip_output(str,indent(nspaces))
          end
          cmd.lstrip.chomp
        end

      end
    end
  end
end
