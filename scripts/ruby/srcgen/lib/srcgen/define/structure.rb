require 'fortran'
require 'define/base'
module SrcGen
  module Define

    class Structure < SrcGen::Define::Base

      BODY_INDENT = " "*4
      
      
      # Method to generate the structure definition
      def generate(debug=false)
      
        # Construct the definition string
        str = <<-EOT
  ! #{config.name} data type definition
  !:tdoc+:
  TYPE :: #{config.type_name}
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
#{construct_releaseversion.join}
#{construct_dimensions.join}
#{construct_dimvectors.join}
#{construct_arraydata.join}
  END TYPE #{config.type_name}
  !:tdoc-:
        EOT
        
        # Output definition in debug mode
        if debug
          puts("\n---BEGIN-DEBUG-OUTPUT---")
          puts("\n#{self.class} #{__method__} method output:")
          puts("\n#{str}")
          puts("\n----END-DEBUG-OUTPUT----")
        end
        
        str
      end
      
    private
    
      def construct_releaseversion
        str = ["#{BODY_INDENT}! Release and version information\n"]
        ["Release", "Version"].each do |p|
          str << Fortran::BaseType.new("INTEGER", p,
                                       :kind    => "Long",
                                       :value   => "#{config.name.upcase}_#{p.upcase}",                                      :indent      => BODY_INDENT,
                                       :indent  => BODY_INDENT,
                                       :newline => true).definition
        end
        str[-1].rstrip!
        str
      end
      
      
      def construct_dimensions
        max_dim_name_length = config.max_dim_name_length
        str = ["#{BODY_INDENT}! Dimensions\n"]
        config.dimensions.each do |dim|
          name_align = max_dim_name_length - dim[:name].length
          str << Fortran::BaseType.new("INTEGER", dim[:name], 
                                       :kind  => "Long", 
                                       :value => "0",
                                       :name_align  => name_align, 
                                       :description => "I#{dim[:index]} dimension",
                                       :indent      => BODY_INDENT,
                                       :newline     => true).definition
        end
        str[-1].rstrip!
        str
      end
      
      def construct_dimvectors
        max_dimvec_name_length = config.max_dimvec_name_length
        str = ["#{BODY_INDENT}! Dimensional vectors\n"]
        config.dimvectors.each do |vec|
          name_align = max_dimvec_name_length - vec[:name].length
          str << Fortran::BaseType.new(vec[:type], vec[:name], 
                                       :kind  => vec[:kind],
                                       :attributes => "ALLOCATABLE", 
                                       :dimspec    => "(:)",
                                       :name_align  => name_align, 
                                       :description => "I#{vec[:dimindex]}",
                                       :indent      => BODY_INDENT,
                                       :newline     => true).definition
        end
        str[-1].rstrip!
        str
      end

      def construct_arraydata
        str = ["#{BODY_INDENT}! Array data\n"]
        config.arraydata.each do |array|
          dimspec = "("+array[:dimindex].gsub(/[0-9]/,":")+")"
          description = array[:dimindex].split(",").collect {|d| "I#{d}"}.join(" x ")
          str << Fortran::BaseType.new(array[:type], array[:name], 
                                       :kind  => array[:kind],
                                       :attributes => "ALLOCATABLE", 
                                       :dimspec    => dimspec,
                                       :description => description,
                                       :indent      => BODY_INDENT,
                                       :newline     => true).definition
        end
        str[-1].rstrip!
        str
      end


    end
  end
end
