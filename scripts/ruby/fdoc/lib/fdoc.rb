# Put fdoc heirarchy at
# beginning of search path
$:.unshift File.join(File.dirname(__FILE__))

require 'source/source'
require 'base/base'
require 'type/type'
require 'procedure/procedure'

module FDoc
  
  # Main driver of the document generation
  class Generator
  
    # Generators for individual parts
    # Just add another class to generate
    # another part
    GENERATORS = [
      FDoc::Type::Generator,
      FDoc::Procedure::Generator
    ]
    
    def generate(source)
      GENERATORS.each do |gen_class|
        gen = gen_class.new(source)
        gen.generate
      end
    end
  end
end
