module FGenMod

  # FGenMod base class with all shared bits of code
  class Base
    RSTR="  "
    INDENT=4
    attr_accessor :config

    # Method to return just
    # the class name
    def class_name
      self.class.to_s.split("::").last
    end
    
    # Method to return the
    # deepest module name
    def module_name
      self.class.to_s.split("::")[-2]
    end
    
    # Generate a procedure name
    # based on class and module
    def procedure_name
      module_tag = self.module_name=="Define" ? "" : "_" << self.module_name
      "#{config.namespace}#{self.class_name}_#{config.struct_name}#{module_tag}"
    end

    # Method to construct the
    # list of public procedures
    # from a generator class list
    def public_procedures(class_array,args={})
      nspaces = args[:nspaces] ? args[:nspaces] : 0
      str=""
      class_array.each do |gen_class|
        gen=gen_class.new
        gen.config=self.config
        str<<indent(nspaces)<<"PUBLIC :: "<<gen.procedure_name<<"\n"
      end
      str.lstrip.chomp
    end
  
    # Generate the structure
    # association test
    def struct_assoc_test(suffix="",args={})
      nspaces = args[:nspaces] ? args[:nspaces] : 0
      (assoc=Define::Assoc.new).config=self.config
      struct_name=config.struct_name+suffix
      cmd ="IF ( .NOT. #{assoc.procedure_name}( #{struct_name} ) ) THEN\n"
      cmd<<fail_return("'Some or all INPUT #{struct_name} pointer members are NOT associated.'",:nspaces=>nspaces+2)+"\n"
      cmd<<indent(nspaces)<<"END IF"
    end

    # Method to construct the dimension
    # argument definitions in the procedure
    # declaration
    def procedure_dim_def(dfmt,args={})
      nspaces = args[:nspaces] ? args[:nspaces] : 0
      desc = args[:optional] ? "Optional " : ""
      desc << (args[:out] ? "Output" : "Input")
      str=""
      config.dim_list.each {|d| str<<indent(nspaces)<<"#{dfmt%d}, &  ! #{desc}\n"}
      str.lstrip.chomp  # Strip leading spaces of first line, newline of last line
    end
    
    # Method to construct the dimension
    # argument definitions in the variable
    # declarations of a procedure
    def argument_dim_def(afmt,args={})
      nspaces   = args[:nspaces] ? args[:nspaces] : 0
      attribute = args[:optional] ? "OPTIONAL, " : ""
      intent    = args[:out] ? "OUT" : "IN"
      pad       = args[:npad] ? " "*args[:npad] : ""   # Keyword arg to pad spaces for other arg intent lineup.
      str=""
      config.dim_list.each {|d| str<<indent(nspaces)<<"#{afmt%"INTEGER"}, #{attribute}INTENT(#{intent}) #{pad}:: #{d}\n"}
      str.lstrip.chomp  # Strip leading spaces of first line, newline of last line
    end
    
    # Method to construct a structure
    # allocation statement
    def alloc_struct(args={})
      nspaces = args[:nspaces] ? args[:nspaces] : 0
      (alloc=Define::Alloc.new).config=self.config
      # Build the pretty output format
      dfmt=string_format(config.dim_list)
      # Build the structure allocation call
      cmd="Error_Status = #{alloc.procedure_name}( "; n=cmd.length+nspaces
      str=""
      config.dim_list.each do |d|
        str<<indent(n)+"#{config.struct_name}_in%#{dfmt%d}, &\n"
      end
      cmd<<str.lstrip
      cmd<<indent(n)<<"#{config.struct_name}_out, &\n"
      cmd<<indent(n)<<"Message_Log=Message_Log )"
    end

    # Generate dimension value
    # check statement
    def dimension_size_test(args={})
      nspaces = args[:nspaces] ? args[:nspaces] : 0
      prefix = args[:prefix] ? "#{config.struct_name}%" : ""
      # Build a formatted list
      dim_list=list_format(config.dim_list.collect {|d| "#{prefix}#{d}"})
      # Build the IF test
      cmd="IF ("; n=cmd.length+nspaces
      if dim_list.length>1
        str=""
        dim_list[0..-2].each {|d| str<<indent(n)<<"#{d} < 1 .OR. &\n"}
        cmd<<str.lstrip<<indent(n)<<"#{dim_list.last} < 1) THEN\n"
      else
        cmd<<"#{dim_list.first} < 1) THEN\n"
      end
      cmd<<fail_return("'Input #{config.struct_name} dimensions must all be > 0.'",:nspaces=>nspaces+2)+"\n"
      cmd<<indent(nspaces)<<"END IF"
    end


    # --------------------------------------
    # The following methods don't reference
    # instances (so could be class methods?)
    # --------------------------------------
    # Method to construct the list of
    # dependencies, supplied in the hash
    # array argument, in the module headers
    def dependencies(hash_array,args={})
      nspaces = args[:nspaces] ? args[:nspaces] : 0
      # Get a pretty print format
      fmt=string_format(hash_array.collect {|d| d[:mod]})
      # Build the module use list
      str=""
      hash_array.each do |d|
        str<<indent(nspaces)<<"USE #{fmt%d[:mod]}, ONLY: #{d[:only_list].join(", ")}\n"
      end
      str.lstrip.chomp
    end

    # Generate output format
    # for an array of names
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
