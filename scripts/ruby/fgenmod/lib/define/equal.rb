require 'define/base'
module FGenMod
  module Define
  
    class Equal < FGenMod::Define::Base
    
      def generate
        name=procedure_name
        type="TYPE(#{config.namespace}#{config.struct_name}_type)"

        # Declaration and argument type definition format
        dfmt=string_format(["#{config.struct_name}_LHS","Message_Log"])
        afmt=string_format([type,"CHARACTER(*), OPTIONAL"])

        # Base nspaces indent
        nspaces=12
        
        # The function
        str=strip_output(<<-EOT)
          FUNCTION #{name}( &
                     #{dfmt%"#{config.struct_name}_LHS"}, &  ! Input
                     #{dfmt%"#{config.struct_name}_RHS"}, &  ! Input
                     #{dfmt%"ULP_Scale"}, &  ! Optional input
                     #{dfmt%"RCS_Id"}, &  ! Revision control
                     #{dfmt%"Message_Log"}) &  ! Error messaging
                   RESULT( Error_Status )
            ! Arguments
            #{afmt%type}, INTENT(IN)  :: #{config.struct_name}_LHS
            #{afmt%type}, INTENT(IN)  :: #{config.struct_name}_RHS
            #{afmt%"INTEGER,      OPTIONAL"}, INTENT(IN)  :: ULP_Scale
            #{afmt%"CHARACTER(*), OPTIONAL"}, INTENT(OUT) :: RCS_Id
            #{afmt%"CHARACTER(*), OPTIONAL"}, INTENT(IN)  :: Message_Log
            ! Function result
            INTEGER :: Error_Status
            ! Local parameters
            CHARACTER(*), PARAMETER :: ROUTINE_NAME = '#{name}'
            ! Local variables
            CHARACTER(256) :: Message
            INTEGER :: ULP
            INTEGER :: l, j
            INTEGER :: #{config.dimdecl.join(",")}

            ! Set up
            Error_Status = SUCCESS
            IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

            ! Default precision is a single unit in last place
            ULP = 1
            ! ... unless the ULP_Scale argument is set and positive
            IF ( PRESENT( ULP_Scale ) ) THEN
              IF ( ULP_Scale > 0 ) ULP = ULP_Scale
            END IF

            ! Check the structure association status
            #{struct_associated_test("_LHS",:nspaces=>nspaces)}
            #{struct_associated_test("_RHS",:nspaces=>nspaces)}
            
            ! Check dimensions
            #{dim_equal_test(:nspaces=>nspaces)}
              #{fail_return("'Structure dimensions are different.'",:nspaces=>nspaces+2,:lstrip=>true)}
            END IF

            ! Check the scalar components
            #{scalar_equal_test(:nspaces=>nspaces)}
            
            ! Check the array components
            #{array_equal_test(:nspaces=>nspaces)}
            
          END FUNCTION #{name}
        EOT
      end
  
      
      # --------------
      # helper methods
      # --------------
      # Method to construct the dimension
      # equality IF test in the equal function
      def dim_equal_test(args={})
        nspaces = args[:nspaces] ? args[:nspaces] : 0
        s=config.struct_name
        # Build a formatted list
        dim_list=list_format(config.dim_list)
        # Build the IF statement
        cmd="IF ("; n=cmd.length+nspaces
        if dim_list.length>1
          str=""
          dim_list[0..-2].each do |d|
            str<<indent(n)<<"#{s}_LHS%#{d} /= #{s}_RHS%#{d} .OR. &\n"
          end
          cmd<<str.lstrip<<indent(n)<<"#{s}_LHS%#{dim_list.last} /= #{s}_RHS%#{dim_list.last}) THEN"
        else
          cmd<<"#{s}_LHS%#{dim_list.first} /= #{s}_RHS%#{dim_list.first}) THEN"
        end
      end

      # Method to construct the scalar
      # component equality checks
      # in the equal function
      def scalar_equal_test(args={})
        nspaces = args[:nspaces] ? args[:nspaces] : 0
        sn=config.struct_name
        # Build the IF test
        cmd=""
        config.scalar_list.each do |s|
          case s[:type]
            when "INTEGER","CHARACTER","LOGICAL"
              # Integers, characters, and
              # logicals can be directly
              # compared
              op = s[:type]=="LOGICAL" ? ".NEQV." : "/="
              str =indent(nspaces)<<"IF ( #{sn}_LHS%#{s[:name]} #{op} #{sn}_RHS%#{s[:name]} ) THEN\n"
              str<<fail_return("'#{sn} scalar component #{s[:name]} values are different.'",:nspaces=>nspaces+2)+"\n"
              str<<indent(nspaces)+"END IF\n"
              cmd<<str
              
            when "REAL","COMPLEX"
              # Floating point values
              # use Compare_Float
              str =indent(nspaces)<<"IF ( .NOT. Compare_Float( "; n=str.length
              str<<"#{sn}_LHS%#{s[:name]}, &\n"
              str<<indent(n)<<"#{sn}_RHS%#{s[:name]}, &\n"
              str<<indent(n)<<"ULP=ULP ) ) THEN\n"
              str<<fail_return("'#{sn} scalar component #{s[:name]} values are different.'",:nspaces=>nspaces+2)+"\n"
              str<<indent(nspaces)+"END IF\n"
              cmd<<str
              
            else
              # Other data types not supported
              raise StandardError, "*** Type #{s[:type]} scalar equality test not yet implemented ***"
          end
        end
        cmd.lstrip.chomp  # Strip leading spaces of first line, newline of last line
      end

      # Method to construct the array
      # component equality checks
      # in the equal function
      #
      # Build the loop DO statements
      def begin_loop(sn,a,args={})
        nspaces = args[:nspaces] ? args[:nspaces] : 0
        cmd=""
        0.upto(a[:ndims]-1) do |i|
          n=nspaces+(i*2)
          cmd<<indent(n)<<"DO #{a[:dimidx].reverse[i]}=#{a[:dims].reverse[i][0]},#{sn}_LHS%#{a[:dims].reverse[i][1]}\n"
        end
        cmd
      end
      #
      # Build the loop END DO statements
      def end_loop(a,args={})
        nspaces = args[:nspaces] ? args[:nspaces] : 0
        cmd=""
        (a[:ndims]-1).downto(0) do |i|
          cmd<<indent(nspaces+(i*2))<<"END DO\n"
        end
        cmd
      end
      #
      # The main method
      def array_equal_test(args={})
        nspaces = args[:nspaces] ? args[:nspaces] : 0
        sn=config.struct_name
        # Build the IF test
        cmd=""
        config.array_list.each do |a|
          case a[:type]
            when "INTEGER","CHARACTER","LOGICAL"
              # Integers, characters, and
              # logicals can be directly
              # compared
              op = a[:type]=="LOGICAL" ? ".NEQV." : "/="
              str =indent(nspaces)<<"IF ( ANY( #{sn}_LHS%#{a[:name]} #{op} #{sn}_RHS%#{a[:name]} ) ) THEN\n"
              str<<fail_return("'#{sn} array component #{a[:name]} values are different.'",:nspaces=>nspaces+2)+"\n"
              str<<indent(nspaces)+"END IF\n"
              cmd<<str

            when "REAL","COMPLEX"
              # Floating point values
              # use Compare_Float
              nin = a[:ndims]*2 + nspaces
              cmd<<begin_loop(sn,a,:nspaces=>nspaces)
              str =indent(nin)<<"IF ( .NOT. Compare_Float( "; n=str.length
              str<<"#{sn}_LHS%#{a[:name]}(#{a[:dimidx].join(",")}), &\n"
              str<<indent(n)<<"#{sn}_RHS%#{a[:name]}(#{a[:dimidx].join(",")}), &\n"
              str<<indent(n)<<"ULP=ULP ) ) THEN\n"
              str<<indent(nin+2)<<%Q{WRITE(Message,'("#{config.struct_name} array component #{a[:name]} values ",&\n}
              str<<indent(nin+2)<<%Q{                &"are different at index (",#{a[:ndims]}(1x,i0),")")') &\n}
              str<<indent(nin+2)<<%Q{                #{a[:dimidx].join(",")}\n}
              str<<fail_return("TRIM(Message)",:nspaces=>nin+2)+"\n"
              str<<indent(nin)<<"END IF\n"
              cmd<<str
              cmd<<end_loop(a,:nspaces=>nspaces)
              
            else
              # Non-implemented types
              raise StandardError, "*** Type #{a[:type]} array equality test not yet implemented ***"
          end
        end
        cmd.lstrip.chomp  # Strip leading spaces of first line, newline of last line
      end
    end
  end
end
