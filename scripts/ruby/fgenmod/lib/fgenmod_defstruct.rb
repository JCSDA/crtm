require 'fgenmod_defbase'
module FGenMod
  module Def

    class Struct < FGenMod::Def::Base

      def generate
        type="#{config.namespace}#{config.struct_name}_type"

        # Base nspaces indent is 12
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
      # Method to construct the release and version definitions
      # in the structure definition function
      def release_version
        defn=""
        if config.release and config.version
          defn ="! Release and version information\n"
          defn<<"    INTEGER :: Release = #{config.struct_name.upcase}_RELEASE\n"
          defn<<"    INTEGER :: Version = #{config.struct_name.upcase}_VERSION"
        end
      end

      # Method to construct the dimension definitions
      # in the structure definition function
      def dimensions
        defn="! Dimensions\n"
        list_format(config.dim_list).each {|d| defn<<"    INTEGER :: #{d} = 0\n"}
        defn.chomp  # Remove the last newline
      end
    
      # Method to construct the scalar component definitions
      # in the structure definition function
      def scalars
        defn=""
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
            defn<<"    #{tlist[i]} :: #{slist[i]}#{ilist[i]}#{dlist[i]}\n"
          end
        end
        defn.chomp  # Remove the last newline
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
