require 'define/base'
module FGenMod
  module Define

    class Struct < FGenMod::Define::Base

      def generate
        type="#{config.namespace}#{config.struct_name}_type"

        # Base nspaces indent
        nspaces=12
        
        # The function
        str=strip_output(<<-EOT)
          ! -----------------------
          ! Derived type definition
          ! -----------------------
          TYPE :: #{type}
            INTEGER :: n_Allocates=0
            #{release_version()}
            #{dimensions()}
            #{scalars()}
            #{arrays()}
          END TYPE #{type}
        EOT
      end

      # --------------
      # helper methods
      # --------------
      # Method to construct the release
      # and version definitions in the
      # structure definition function
      def release_version(args={})
        nspaces = args[:nspaces] ? args[:nspaces] : 0

        # Construct the definitions
        defn=""
        if config.release and config.version
          defn  = "! Release and version information\n"
          defn << indent(nspaces)<<"INTEGER :: Release = #{config.struct_name.upcase}_RELEASE\n"
          defn << indent(nspaces)<<"INTEGER :: Version = #{config.struct_name.upcase}_VERSION"
        end
      end

      # Method to construct the dimension
      # definitions in the structure
      # definition
      def dimensions(args={})
        nspaces = args[:nspaces] ? args[:nspaces] : 0

        # Construct the definitions
        defn = "! Dimensions\n"
        list_format(config.dim_list).each do |d|
          defn << indent(nspaces)<<"INTEGER :: #{d} = 0\n"
        end
        defn.chomp
      end
    
      # Method to construct the scalar
      # component definitions in the
      # structure definition
      def scalars(args={})
        nspaces = args[:nspaces] ? args[:nspaces] : 0
        
        # Only construct definitions
        # if there are any scalars defined
        unless config.scalar_list.empty?
          # Build the formatted output lists
          # so everything is lined up
          tlist=list_format(config.scalar_list.collect do |s|
                              %Q{#{s[:type]}#{"(#{s[:param]})" unless s[:param].nil?}}
                            end)
          slist=list_format(config.scalar_list.collect {|s| s[:name]})
          ilist=list_format(config.scalar_list.collect {|s| s[:initval] ? " = #{s[:initval]}" : ""})
          dlist=config.scalar_list.collect {|s| s[:desc] ? " ! #{s[:desc]}" : ""}
          # Construct the scalar definitions
          defn="! Scalars\n"
          tlist.each_index do |i|
            defn << indent(nspaces)<<"#{tlist[i]} :: #{slist[i]}#{ilist[i]}#{dlist[i]}\n"
          end
        else
          defn = ""
        end
        defn.chomp
      end


      # Method to construct the array component definitions
      # in the structure definition function
      def arrays
        # Build the formatted output lists
        # so everything is lined up
        tlist=list_format(config.array_list.collect do |a|
                            %Q{#{a[:type]}#{"(#{a[:param]})" unless a[:param].nil?}}
                          end)
        clist=list_format(config.array_list.collect do |a|
                            "("<<([":"]*a[:ndims]).join(",")<<")"
                          end)
        alist=list_format(config.array_list.collect {|a| a[:name]})
        dlist=config.array_list.collect {|a| a[:desc] ? " ! #{a[:desc]}" : ""}
        # Construct the array definitions
        defn="! Arrays\n"
        tlist.each_index do |i|
          defn<<"    #{tlist[i]}, DIMENSION#{clist[i]}, POINTER :: #{alist[i]} => NULL()#{dlist[i]}\n"
        end
        defn.chomp  # Remove the last newline
      end
    end
  end
end
