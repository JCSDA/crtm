require 'base/base'
module FGenMod
  module IO
    module Binary
      class Base < FGenMod::IO::Base

        # This class is how the config gets passed along
        # and hold shared IO::Binary code

        # Method to construct the Binary file
        # open function call
        def open_binary_file(args={})
          nspaces = args[:nspaces] ? args[:nspaces] : 0

          # Construct the command
          cmd  = "Error_Status = Open_Binary_File( TRIM(Filename), &\n"
          cmd << indent(nspaces)<<"                                 FileID, &\n"
          cmd << indent(nspaces)<<"                                 For_Output =SET, &\n" if io_action=="write"
          cmd << indent(nspaces)<<"                                 Message_Log=Message_Log)\n"
          cmd << indent(nspaces)<<"IF ( Error_Status /= SUCCESS ) THEN\n"
          cmd << fail_return("'Error opening #{config.struct_name} file '//TRIM(Filename)",:nspaces=>nspaces+2)<<"\n"
          cmd << indent(nspaces)<<"END IF"
        end


        # Method to construct the Binary
        # file close statement
        def close_binary_file(args={})
          nspaces = args[:nspaces] ? args[:nspaces] : 0

          # Construct the command
          cmd  = "CLOSE( FileID, STATUS='KEEP', &\n"
          cmd << indent(nspaces)<<"       IOSTAT = IO_Status )\n"
          cmd << indent(nspaces)<<"IF ( IO_Status /= 0 ) THEN\n"
          cmd << indent(nspaces)<<"  WRITE(Message,'(\"Error closing \", a, \". IOSTAT = \", i0 )' ) &\n"
          cmd << indent(nspaces)<<"                TRIM(Filename), IO_Status\n"
          cmd << fail_return("TRIM(Message)",:nspaces=>nspaces+2)<<"\n"
          cmd << indent(nspaces)<<"END IF"
        end


        # Method to contruct the Binary IO
        # read and write statements for
        # the release and version info
        def release_version_io(args={})
          nspaces = args[:nspaces] ? args[:nspaces] : 0

          # Build the I/O list
          unless io_action == "inquire"
            io_list = "#{config.struct_name}%Release, #{config.struct_name}%Version"
            io_cmd  = io_action
          else
            io_list = "Rel, Ver"
            io_cmd  = "read"
          end

          # Construct the command
          cmd  = "#{io_cmd.upcase}(FileID, IOSTAT=IO_Status) #{io_list}\n"
          cmd << indent(nspaces)<<"IF ( IO_Status /= 0 ) THEN\n"
          cmd << indent(nspaces)<<"  WRITE(Message,'(\"Error #{io_ing} #{config.struct_name} Release/Version values #{io_tofrom} \", a, &\n"
          cmd << indent(nspaces)<<"                 &\". IOSTAT = \", i0 )' ) &\n"
          cmd << indent(nspaces)<<"                 TRIM(Filename), IO_Status\n"
          cmd << indent(nspaces)<<"  CLOSE(FileID)\n"
          cmd << fail_return("TRIM(Message)",:nspaces=>nspaces+2)<<"\n"
          cmd << indent(nspaces)<<"END IF"
        end


        # Method to construct the Binary IO
        # release check statements
        def check_release(args={})
          nspaces = args[:nspaces] ? args[:nspaces] : 0

          # Create a checkrelease object
          (checkrelease = Define::CheckRelease.new).config = self.config

          # Construct the command
          cmd  = "Error_Status = #{checkrelease.procedure_name}( &\n"
          cmd << indent(nspaces)<<"                 #{config.struct_name}, &\n"
          cmd << indent(nspaces)<<"                 Message_Log=Message_Log)\n"
          cmd << indent(nspaces)<<"IF ( Error_Status /= SUCCESS ) THEN\n"
          cmd << indent(nspaces)<<"  CLOSE(FileID)\n"
          cmd << fail_return("'#{config.struct_name} Release check failed for '//TRIM(Filename)",:nspaces=>nspaces+2)<<"\n"
          cmd << indent(nspaces)<<"END IF\n"
        end


        # Method to construc the Binary IO
        # read and write statements for 
        # the dimensions
        def dimension_io(args={})
          nspaces = args[:nspaces] ? args[:nspaces] : 0

          # Build the I/O list
          unless io_action == "inquire"
            io_list = config.dim_list.collect {|d| "#{config.struct_name}%#{d}"}.join(", ")
            io_cmd  = io_action
          else
            io_list = config.dimdecl.join(",")
            io_cmd  = "read"
          end

          # Construct the command
          cmd  = "#{io_cmd.upcase}(FileID, IOSTAT=IO_Status) &\n"
          cmd << indent(nspaces)<<"  #{io_list}\n"
          cmd << indent(nspaces)<<"IF ( IO_Status /= 0 ) THEN\n"
          cmd << indent(nspaces)<<"  WRITE(Message,'(\"Error #{io_ing} #{config.struct_name} dimension values #{io_tofrom} \", a, &\n"
          cmd << indent(nspaces)<<"                 &\". IOSTAT = \", i0 )' ) &\n"
          cmd << indent(nspaces)<<"                 TRIM(Filename), IO_Status\n"
          cmd << indent(nspaces)<<"  CLOSE(FileID)\n"
          cmd << fail_return("TRIM(Message)",:nspaces=>nspaces+2)<<"\n"
          cmd << indent(nspaces)<<"END IF"
        end


        # Method to construct the Binary IO
        # read and write statements for the
        # scalar and array structure components      
        def component_io(kind,args={})
          nspaces = args[:nspaces] ? args[:nspaces] : 0

          # Determine the kind of
          # structure component
          case kind.downcase
            when "scalar": list = config.scalar_list.collect {|s| s[:name]}
            when "array" : list = config.array_list.collect  {|a| a[:name]}
            else raise ArgumentError, "Invalid component kind specified: #{kind}"
          end

          # Construct the command
          cmd=""
          list.each_index do |i|
            cmd << indent(nspaces)<<"#{io_action.upcase}(FileID, IOSTAT=#{kind.capitalize}_IO_Status(#{i+1})) #{list[i]}\n"
          end
          cmd << indent(nspaces)<<"IF ( ANY( #{kind.capitalize}_IO_Status /= 0 ) ) THEN\n"
          cmd << indent(nspaces)<<"  WRITE(Message,'(\"Error #{io_ing} #{config.struct_name} #{kind} variable(s) \",#{list.length}(1x,i0,:))')\n"
          cmd << indent(nspaces)<<"                PACK( (/(i,i=1,#{list.length})/), &\n"
          cmd << indent(nspaces)<<"                      #{kind.capitalize}_IO_Status /= 0 )\n"
          cmd << indent(nspaces)<<"  CLOSE(FileID)\n"
          cmd << fail_return("TRIM(Message)",:nspaces=>nspaces+2)<<"\n"
          cmd << indent(nspaces)<<"END IF"
          cmd.lstrip
        end

        # Method to construct the Binary IO
        # info output statements
        def info_message(args={})
          nspaces = args[:nspaces] ? args[:nspaces] : 0

          # Create an info object
          (info = Define::Info.new).config = self.config

          # Construct the command
          cmd  = "IF ( Noisy ) THEN\n"
          cmd << indent(nspaces)<<"  CALL #{info.procedure_name}( #{config.struct_name}, Message )\n"
          cmd << indent(nspaces)<<"  CALL Display_Message( ROUTINE_NAME, &\n"
          cmd << indent(nspaces)<<"                        'FILE: '//TRIM(Filename)//'; '//TRIM(Message), &\n"
          cmd << indent(nspaces)<<"                        INFORMATION, &\n"
          cmd << indent(nspaces)<<"                        Message_Log=Message_Log )\n"
          cmd << indent(nspaces)<<"END IF"
        end
      end
    end
  end
end
