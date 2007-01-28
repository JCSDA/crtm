require 'io/base'
require 'io/binary/binary'
require 'io/netcdf/netcdf'

module FGenMod
  module IO
  
    # Main driver of the IO generation
    class Generator < FGenMod::IO::Base
  
      # Generators for individual parts
      # Just add another class to generate
      # another part
      GENERATORS = [
        FGenMod::IO::Binary::Generator,
        FGenMod::IO::NetCDF::Generator
      ]
      
      def generate
        
        GENERATORS.each do |gen_class|
          gen = gen_class.new
          gen.config = self.config
          gen.generate
        end
      end
    end
  end
end
