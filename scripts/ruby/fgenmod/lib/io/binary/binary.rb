require 'io/binary/base'
require 'io/binary/header'
require 'io/binary/inquire'
require 'io/binary/read'
require 'io/binary/write'

module FGenMod
  module IO
    module Binary
  
      class Generator < FGenMod::IO::Binary::Base
        
        DEPENDENCIES=[
          {:mod=>"Type_Kinds",:only_list=>%w{ fp }},
          {:mod=>"File_Utility",:only_list=>%w{ Get_Lun }},
          {:mod=>"Binary_File_Utility",:only_list=>%w{ Open_Binary_File }},
          {:mod=>"Message_Handler",:only_list=>%w{ SUCCESS FAILURE INFORMATION Display_Message }},
          {:mod=>"Compare_Float_Numbers",:only_list=>%w{ Compare_Float }}
        ]

        PRIVATE_GENERATORS=[
        ]
        
        PUBLIC_GENERATORS = [
          Inquire,
          Read,
          Write
        ]
        
        # Method to generate the
        # structure definition module
        def generate
        
          # Output the module header
          h=Header.new
          h.config = self.config
          mod=h.generate
          
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
          mod<<"\nEND MODULE #{self.config.namespace}#{self.config.struct_name}_#{self.io_type}_IO\n"

          # Output to file
          filename = "#{self.config.namespace}#{self.config.struct_name}_#{self.io_type}_IO.f90"
          open(filename,'w') {|f| f.puts(mod)}
        end
      end
    end
  end
end
