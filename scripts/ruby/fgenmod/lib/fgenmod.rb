# Put fgenmod heirarchy at
# beginning of search path
$:.unshift File.join(File.dirname(__FILE__))

require 'config/config'
require 'base/base'
require 'define/define'
require 'io/io'

module FGenMod
  
  # Main driver of the generation
  class Generator
  
    # Generators for individual parts
    # Just add another class to generate
    # another part
    GENERATORS = [
      FGenMod::Define::Generator,
      FGenMod::IO::Generator
    ]
    
    def generate(config_file_name)
      config = Config.load(config_file_name)
      
      GENERATORS.each do |gen_class|
        gen = gen_class.new
        gen.config = config
        gen.generate
      end
    end
  end
end
