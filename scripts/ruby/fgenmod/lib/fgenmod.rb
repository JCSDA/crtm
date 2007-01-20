require 'fgenmod_config'
require 'fgenmod_base'
require 'fgenmod_def'

module FGenMod
  
#  class BinIOMod < FGenMod::Base
#    def generate
#      puts self.class
#    end
#  end
#  
#  class NCIOMod < FGenMod::Base
#    def generate
#      puts self.class
#    end
#  end

  
  # Main driver of the generation
  class Generator
  
    # Generators for individual parts
    # Just add another class to generate
    # another part
    GENERATORS = [
      FGenMod::Def::Generator
    ]
#    GENERATORS = [
#      FGenMod::Def::Generator,
#      FgenMod::BinIO::Generator,
#      FGenMod::NCIO::Generator
#    ]
    
    def generate(config_file_name)
      config = Config.load(config_file_name)
      
      mod=""
      GENERATORS.each do |gen_class|
	gen = gen_class.new
	gen.config = config
	gen.generate
      end
    end
  end
end
