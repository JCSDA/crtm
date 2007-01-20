require 'fgenmod_defbase'
require 'fgenmod_defheader'
require 'fgenmod_defstruct'
require 'fgenmod_defclear'
require 'fgenmod_defassoc'
require 'fgenmod_defdestroy'
require 'fgenmod_defalloc'
require 'fgenmod_defassign'
require 'fgenmod_defequal'
require 'fgenmod_definfo'
require 'fgenmod_defcheckrelease'

module FGenMod
  module Def
  
    class Generator < FGenMod::Def::Base
      
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
        mod<<"\nEND MODULE #{self.config.namespace}#{self.config.struct_name}_Define\n"

        # Output to file
        filename = "#{self.config.namespace}#{self.config.struct_name}_Define.f90"
        open(filename,'w') {|f| f.puts(mod)}
      end
    end
  end
end
