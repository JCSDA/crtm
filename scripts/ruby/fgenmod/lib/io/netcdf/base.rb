require 'base/base'
module FGenMod
  module IO
    module NetCDF
      class Base < FGenMod::IO::Base

        # This class is how the config
        # gets passed along and hold
        # shared NetCDF IO code
        
        # Global attributes for the file
        INTERNAL_GATTS = %w{ Creation_Date Write_Module_History }.freeze
        DEFAULT_GATTS  = %w{ Title History Comment }
        
        # Variable attributes
        DEFAULT_VATTS = %w{ long_name units description _FillValue }
        
        # method to construct the global
        # attribute argument entries in
        # the procedure definition
        def procedure_gatts_def(gfmt,args={})
          nspaces = args[:nspaces] ? args[:nspaces] : 0
          gatts = (DEFAULT_GATTS + config.gatts).uniq
          str=""
          gatts.each {|g| str<<indent(nspaces)<<"#{gfmt%g}, &  ! Optional #{io_dir}\n"}
          str.lstrip.chomp
        end
        
        # method to construct the global
        # attribute argument entries in
        # the argument definitions
        def argument_gatts_def(afmt,args={})
          nspaces = args[:nspaces] ? args[:nspaces] : 0
          gatts = (DEFAULT_GATTS + config.gatts).uniq
          str=""
          gatts.each {|g| str<<indent(nspaces)<<"#{afmt%"CHARACTER(*), OPTIONAL"}, INTENT(#{io_intent}) :: #{g}\n"}
          str.lstrip.chomp
        end

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
            cmd << gatts_io(g_variable,:nspaces=>nspaces)
          end
          cmd.chomp
        end


        # Method to create the read and
        # write statements for user
        # specified global attributes
        def user_gatts_io(args={})
          nspaces = args[:nspaces] ? args[:nspaces] : 0
          
          # Construct array of all user
          # specified global attributes
          gatts = (DEFAULT_GATTS + config.gatts).uniq

          # Build the I/O list
          if io_action == "write"
            nf_action = "PUT"
          else
            nf_action = "GET"
          end

          # Construct the statements for
          # user specified global attributes
          # Conditional output so no heredoc
          cmd = ""
          gatts.each do |g|
            g_parameter = "#{g.upcase}_GATTNAME"
            g_variable  = g
            cmd << indent(nspaces)<<"IF ( PRESENT(#{g}) ) THEN\n"
            cmd << indent(nspaces)<<"  #{g} = ' '\n" if io_action=="read"
            cmd << indent(nspaces)<<"  Long_String = ' '\n" if io_action=="read"
            cmd << gatts_io(g_variable,:nspaces=>nspaces+2)
            cmd << indent(nspaces)<<"  CALL Remove_Null(Long_String)\n" if io_action=="read"
            cmd << indent(nspaces)<<"  #{g} = Long_String(1:MIN(LEN(#{g}),LEN_TRIM(Long_String)))\n" if io_action=="read"
            cmd << indent(nspaces)<<"END IF\n"
          end
          cmd.lstrip.chomp
        end

        # Method to create the call to
        # the global attribute read or
        # write function
        def call_gatts_io(args={})
          nspaces = args[:nspaces] ? args[:nspaces] : 0
          
          # Construct array of all user
          # specified global attributes
          gatts = (DEFAULT_GATTS + config.gatts).uniq
          
          # Pretty print output format 
          # for optional arguments
          msg_log = "Message_Log"
          afmt = string_format(gatts+[msg_log])

          # Construct the call
          cmd  = "Error_Status = #{io_action.capitalize}_GAtts( "; n = cmd.length + nspaces
          cmd << "NC_Filename, &\n"
          cmd << indent(n)<<"NC_FileID  , &\n"
          gatts.each do |g|
            cmd << indent(n)<<"#{afmt%g}=#{afmt%g}, &\n"
          end
          cmd << indent(n)<<"#{afmt%msg_log}=#{afmt%msg_log} )\n"
          cmd << indent(nspaces)<<"IF ( Error_Status /= SUCCESS ) THEN\n"
          cmd << indent(nspaces)<<"  Error_Status = WARNING\n"
          cmd << indent(nspaces)<<"  CALL Display_Message( ROUTINE_NAME, &\n"
          cmd << indent(nspaces)<<"                        'Error #{io_ing} global attributes #{io_tofrom} '// &\n"
          cmd << indent(nspaces)<<"                        TRIM(NC_Filename), &\n"
          cmd << indent(nspaces)<<"                        Error_Status, &\n"
          cmd << indent(nspaces)<<"                        Message_Log=Message_Log )\n"
          cmd << indent(nspaces)<<"END IF"
        end
                  
        
        
        private
        
        
        # Method to create the statement to
        # put or get a global attribute 
        def gatts_io(g_variable,args={})
          nspaces = args[:nspaces] ? args[:nspaces] : 0

          g_parameter = "#{g_variable.upcase}_GATTNAME"
          cmd = <<-EOT
            NF90_Status = NF90_#{nf_action}_ATT( NC_FileID, &
                                        NF90_GLOBAL, &
                                        #{g_parameter}, &
                                        #{g_variable} )
            IF ( NF90_Status /= NF90_NOERR ) THEN
              Error_Status = WARNING
              CALL Display_Message( ROUTINE_NAME, &
                                    'Error #{io_ing} '//#{g_parameter}//' attribute #{io_tofrom} '//&
                                    TRIM( NC_Filename )//' - '// &
                                    TRIM( NF90_STRERROR( NF90_Status ) ), &
                                    Error_Status, &
                                    Message_Log = Message_Log )
            END IF
          EOT
          cmd = strip_output(cmd,indent(nspaces))
        end
        
        # Method to determine the present
        # progressive of the IO action
        # i.e. reading, writing, or inquiring
        # for messages
        def nf_action
          case io_action
            when "read" , "read_gatts"  : "GET"
            when "write", "write_gatts" : "PUT"
          end
        end

      end
    end
  end
end
