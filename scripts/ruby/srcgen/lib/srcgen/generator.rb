# Put current heirarchy at
# beginning of search path
$LOAD_PATH.unshift File.join(File.dirname(__FILE__))

require 'config'
require 'base'
require 'define/define'
#require 'io/io'

module SrcGen
  
  # Main driver of the generation
  class Generator
  
    # Generators for individual parts
    # Just add another class to generate
    # another part
    GENERATORS = [
      SrcGen::Define::Generator
#      SrcGen::IO::Generator
    ]
    
    def generate(config_file, debug=false)
      config = Config.new(config_file, debug=debug)
     
      GENERATORS.each do |gen_class|
        gen = gen_class.new
        gen.config = config
        gen.generate
      end
    end
  end
end
