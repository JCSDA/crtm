require 'define/base'
module FGenMod
  module Define
  
    class Info < FGenMod::Define::Base
    
      def generate
        name=procedure_name
        type="TYPE(#{config.namespace}#{config.struct_name}_type)"

        # Declaration and argument type definition format
        dfmt=string_format(["#{config.struct_name}","RCS_Id"])
        afmt=string_format([type,"CHARACTER(*), OPTIONAL"])

        # Base nspaces indent
        nspaces=12
        
        # The function
        str=strip_output(<<-EOT)
          SUBROUTINE #{name}( &
                       #{dfmt%"#{config.struct_name}"}, &  ! Input
                       #{dfmt%"Info"}, &  ! Output
                       #{dfmt%"RCS_Id"}  )  ! Revision control
            ! Arguments
            #{afmt%type}, INTENT(IN)  :: #{config.struct_name}
            #{afmt%"CHARACTER(*)"}, INTENT(OUT) :: Info
            #{afmt%"CHARACTER(*), OPTIONAL"}, INTENT(OUT) :: RCS_Id
            ! Parameters
            INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
            INTEGER, PARAMETER :: LINEFEED = 10
            ! Local variables
            CHARACTER(80)  :: FmtString
            CHARACTER(512) :: LongString

            ! Set up
            IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

            ! Write the required info to the local string
            #{info_format(:nspaces=>nspaces)}
            #{info_write(:nspaces=>nspaces)}

            ! Trim the output based on the
            ! dummy argument string length
            Info = LongString(1:MIN( LEN(Info), LEN_TRIM(LongString) ))

          END SUBROUTINE #{name}
        EOT
      end
  
      
      # --------------
      # helper methods
      # --------------
      # Method to generate the info output format string
      def info_format(args={})
        nspaces = args[:nspaces] ? args[:nspaces] : 0
        fmt="FmtString='"; n=fmt.length+nspaces
        fmt<<"(a,1x,\"#{config.struct_name}:\",2x,&\n"
        fmt<<config.dim_list.collect {|d| indent(n)<<"&\"#{d.upcase}=\",i0,2x"}.join(",&\n")<<")'"
      end
  
      # Method to generate the info write statement
      def info_write(args={})
        nspaces = args[:nspaces] ? args[:nspaces] : 0
        # Build a formatted list
        dim_list=list_format(config.dim_list)
        # Build the write statement
        cmd="WRITE("; n=cmd.length+nspaces
        cmd<<"LongString, FMT=FmtString) &\n" 
        cmd<<indent(n)<<"ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &\n"
        cmd<<dim_list.collect {|d| indent(n)+"#{config.struct_name}%#{d}"}.join(", &\n")
      end
    end
  end
end
