require 'base/base'
module FGenMod
  module IO
    module NetCDF
      class Base < FGenMod::IO::Base

        # This class is how the config gets
        # passed along and hold shared NCIO code
        
        # Global attributes for the file
        INTERNAL_GATTS = %w{ Creation_Date Write_Module_History }.freeze
        DEFAULT_GATTS  = %w{ Title History Comment }
        
        # Method to create the write statements
        # for the internal global attributes
        def internal_gatts_write(args={})
          nspaces = args[:nspaces] ? args[:nspaces] : 0

          # Create the commands to fill
          # the date and time string
          cmd  = "CALL DATE_AND_TIME( cdate, ctime, czone )\n"
          cmd << indent(nspaces)<<"#{INTERNAL_GATTS[0]} = &\n"
          cmd << indent(nspaces)<<"  cdate(1:4)//'/'//cdate(5:6)//'/'//cdate(7:8)//', '// &\n"
          cmd << indent(nspaces)<<"  ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//' '// &\n"
          cmd << indent(nspaces)<<"  czone//'UTC'\n"
          
          # Construct the statements for
          # the internal global attributes
          INTERNAL_GATTS.each do |g|
            g_parameter = "#{g.upcase}_GATTNAME"
            cmd << indent(nspaces)<<"NF90_Status = NF90_PUT_ATT( NC_FileID, &\n"
            cmd << indent(nspaces)<<"                            NF90_GLOBAL, &\n"
            cmd << indent(nspaces)<<"                            #{g_parameter}, &\n"
            cmd << indent(nspaces)<<"                            #{g} )\n"
            cmd << indent(nspaces)<<"IF ( NF90_Status /= NF90_NOERR ) THEN\n"
            cmd << indent(nspaces)<<"  Error_Status = WARNING\n"
            cmd << indent(nspaces)<<"  CALL Display_Message( ROUTINE_NAME, &\n"
            cmd << indent(nspaces)<<"                        'Error writing '//#{g_parameter}//' attribute to '//&\n"
            cmd << indent(nspaces)<<"                        TRIM( NC_Filename )//' - '// &\n"
            cmd << indent(nspaces)<<"                        TRIM( NF90_STRERROR( NF90_Status ) ), &\n"
            cmd << indent(nspaces)<<"                        Error_Status, &\n"
            cmd << indent(nspaces)<<"                        Message_Log = Message_Log )\n"
            cmd << indent(nspaces)<<"END IF\n"
          end
          cmd.chomp
        end


        # Method to create the write statements
        # for user specified global attributes
        def user_gatts_write(args={})
          nspaces = args[:nspaces] ? args[:nspaces] : 0
          
          # Construct array of all user
          # specified global attributes
          gatts = (DEFAULT_GATTS + config.gatts).uniq

          # Construct the statements for
          # user specified global attributes
          gatts.each do |g|
            g_parameter = "#{g.upcase}_GATTNAME"
            cmd << indent(nspaces)<<"IF ( PRESENT(#{g}) ) THEN\n"
            cmd << indent(nspaces)<<"  NF90_Status = NF90_PUT_ATT( NC_FileID, &\n"
            cmd << indent(nspaces)<<"                              NF90_GLOBAL, &\n"
            cmd << indent(nspaces)<<"                              #{g_parameter}, &\n"
            cmd << indent(nspaces)<<"                              #{g} )\n"
            cmd << indent(nspaces)<<"  IF ( NF90_Status /= NF90_NOERR ) THEN\n"
            cmd << indent(nspaces)<<"    Error_Status = WARNING\n"
            cmd << indent(nspaces)<<"    CALL Display_Message( ROUTINE_NAME, &\n"
            cmd << indent(nspaces)<<"                          'Error writing '//#{g_parameter}//' attribute to '//&\n"
            cmd << indent(nspaces)<<"                          TRIM( NC_Filename )//' - '// &\n"
            cmd << indent(nspaces)<<"                          TRIM( NF90_STRERROR( NF90_Status ) ), &\n"
            cmd << indent(nspaces)<<"                          Error_Status, &\n"
            cmd << indent(nspaces)<<"                          Message_Log = Message_Log )\n"
            cmd << indent(nspaces)<<"  END IF\n"
            cmd << indent(nspaces)<<"END IF\n"
          end
          cmd.lstrip.chomp
        end


        # Method to create the read statements
        # for user specified global attributes
        def user_gatts_read(args={})
          nspaces = args[:nspaces] ? args[:nspaces] : 0
          
          # Construct array of all
          # global attributes
          gatts = (DEFAULT_GATTS + config.gatts).uniq

          # Construct the command
          cmd = ""
          gatts.each do |g|
            g_parameter = "#{g.upcase}_GATTNAME"
            cmd << indent(nspaces)<<"IF ( PRESENT(#{g}) ) THEN\n"
            cmd << indent(nspaces)<<"  #{g} = ' '\n"
            cmd << indent(nspaces)<<"  long_string = ' '\n"
            cmd << indent(nspaces)<<"  NF90_Status = NF90_GET_ATT( NC_FileID, &\n"
            cmd << indent(nspaces)<<"                              NF90_GLOBAL, &\n"
            cmd << indent(nspaces)<<"                              #{g_parameter}, &\n"
            cmd << indent(nspaces)<<"                              long_string )\n"
            cmd << indent(nspaces)<<"  IF ( NF90_Status /= NF90_NOERR ) THEN\n"
            cmd << indent(nspaces)<<"    Error_Status = FAILURE\n"
            cmd << indent(nspaces)<<"    CALL Display_Message( ROUTINE_NAME, &\n"
            cmd << indent(nspaces)<<"                          'Error reading '//#{g_parameter}//' attribute from '//&\n"
            cmd << indent(nspaces)<<"                          TRIM( NC_Filename )//' - '// &\n"
            cmd << indent(nspaces)<<"                          TRIM( NF90_STRERROR( NF90_Status ) ), &\n"
            cmd << indent(nspaces)<<"                          Error_Status, &\n"
            cmd << indent(nspaces)<<"                          Message_Log = Message_Log )\n"
            cmd << indent(nspaces)<<"  END IF\n"
            cmd << indent(nspaces)<<"  CALL Remove_NULL_Characters(long_string)\n"
            cmd << indent(nspaces)<<"  #{g} = long_string(1:MIN(LEN(#{g}),LEN_TRIM(long_string)))\n"
            cmd << indent(nspaces)<<"END IF\n"
          end
          cmd.lstrip.chomp
        end
      end
    end
  end
end
