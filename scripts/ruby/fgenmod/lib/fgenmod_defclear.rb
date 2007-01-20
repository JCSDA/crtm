require 'fgenmod_defbase'
module FGenMod
  module Def
  
    class Clear < FGenMod::Def::Base
    
      def generate
        name=procedure_name("Clear")
        str=strip_output(<<-EOT)
          SUBROUTINE #{name}(#{config.struct_name})
            TYPE(#{config.namespace}#{config.struct_name}_type), INTENT(IN OUT) :: #{config.struct_name}
            #{scalar_clear() unless config.scalar_list.empty?}
          END SUBROUTINE #{name}
        EOT
      end

      # --------------
      # helper methods
      # --------------
      # Method to construct the scalar
      # component reinitialisation
      # statements in the clear subroutine
      def scalar_clear
        # Build the formatted output lists
        slist=list_format(config.scalar_list.collect {|s| s[:initval] ? "#{s[:name]}" : nil }.compact)
        ilist=config.scalar_list.collect {|s| s[:initval] ? " = #{s[:initval]}" : nil}.compact
        defn=""
        slist.each_index do |i|
          defn<<"    #{config.struct_name}%#{slist[i]}#{ilist[i]}\n"
        end
        defn.lstrip.chomp  # Strip leading spaces of first line, newline of last line
      end
    end
  end
end
