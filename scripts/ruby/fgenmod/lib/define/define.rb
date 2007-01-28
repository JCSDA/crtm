require 'define/base'
require 'define/header'
require 'define/struct'
require 'define/clear'
require 'define/assoc'
require 'define/destroy'
require 'define/alloc'
require 'define/assign'
require 'define/equal'
require 'define/info'
require 'define/checkrelease'

module FGenMod
  module Define
  
    class Generator < FGenMod::Define::Base
      
      DEPENDENCIES=[
        {:mod=>"Type_Kinds",:only_list=>%w{ fp }},
        {:mod=>"Message_Handler",:only_list=>%w{ SUCCESS FAILURE Display_Message }},
        {:mod=>"Compare_Float_Numbers",:only_list=>%w{ Compare_Float }}
      ]

      PRIVATE_GENERATORS=[
        Clear
      ]
      
      PUBLIC_GENERATORS = [
        Assoc,
        Destroy,
        Alloc,
        Assign,
        Equal,
        Info,
        CheckRelease
      ]
      
      # Method to generate the
      # structure definition module
      def generate
      
        # Output the module header
        h=Header.new
        h.config = self.config
        mod=h.generate
        
        # Output the structure definition
        s=Struct.new
        s.config = self.config
        mod<<s.generate

        # The module contains statement
        mod<<"\nCONTAINS\n"
        
        # Output the private routines
        PRIVATE_GENERATORS.each do |gen_class|
          gen = gen_class.new
          gen.config = self.config
          mod<<gen.generate
        end
        
        # Output the public routines        
        PUBLIC_GENERATORS.each do |gen_class|
          gen = gen_class.new
          gen.config = self.config
          mod<<gen.generate
        end

        # The module end statement
        mod<<"\nEND MODULE #{self.config.namespace}#{self.config.struct_name}_#{self.module_name}\n"

        # Output to file
        filename = "#{self.config.namespace}#{self.config.struct_name}_#{self.module_name}.f90"
        open(filename,'w') {|f| f.puts(mod)}
      end
    end
  end
end
