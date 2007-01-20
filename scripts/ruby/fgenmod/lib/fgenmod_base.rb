module FGenMod

  # FGenMod base class with all shared bits of code
  class Base
    RSTR="  "
    INDENT=4
    attr_accessor :config

    # Generate procedure names
    def procedure_name(procedure)
      "#{config.namespace}#{procedure}_#{config.struct_name}"
    end
  
    # Generate structure association test
    def struct_assoc_test(suffix="",args={})
      nspaces = args[:nspaces] ? args[:nspaces] : 0
      struct_name=config.struct_name+suffix
      cmd ="IF ( .NOT. #{procedure_name("Associated")}( #{struct_name} ) ) THEN\n"
      cmd<<fail_return("'Some or all INPUT #{struct_name} pointer members are NOT associated.'",:nspaces=>nspaces+2)+"\n"
      cmd<<indent(nspaces)+"END IF"
    end
    
    # Generate output format for an array of names
    def string_format(array)
      str_len=array.inject(0) {|memo,n| memo >= n.length ? memo : n.length}
      "%-#{str_len}.#{str_len}s"
    end
    
    # Generate a formatted list of an array of names
    def list_format(list)
      fmt=string_format(list)
      list.collect {|l| "#{fmt%l}"}
    end
  
    # Method to replace only the first
    # occurance of the leading spaces
    # in each line of input text.
    # Arguments:
    #   text:  the text string for which
    #          leading spaces are to be
    #          stripped
    #   rstr:  the replacement string.
    #          Default is FGenMod::Base::RSTR
    def strip_output(text,rstr=RSTR)
      text =~ /^\s+/
      leading_spaces = $&
      text = text.to_a.collect {|l| l.sub(/^#{leading_spaces}/,rstr)}.to_s
    end
  
    # Generate nspaces indent levels for code
    def indent(nspaces)
      " "*nspaces
    end

    # Generate the error message code
    # block nspaces from left margin
    # Arguments:
    #   :rstr   => string     passed to strip_output
    #   :lstrip => true       passed to strip_output
    def fail_return(message,args={})
      nspaces = args[:nspaces]  ? args[:nspaces] : 0
      str=(<<-EOT
        Error_Status = FAILURE
        CALL Display_Message( &
               ROUTINE_NAME, &
               #{message}, &
               Error_Status, &
               Message_Log=Message_Log )
        RETURN
      EOT
      ).chomp
      str=strip_output(str,indent(nspaces))
      args[:lstrip] ? str.lstrip : str
    end

  end
end
