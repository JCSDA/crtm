require 'define/base'
require 'define/structure'
require 'define/allocated'
require 'define/destroy'
require 'define/create'

module SrcGen
  module Define
  
    class Generator < SrcGen::Define::Base
      
      # Method to generate the structure definition
      def generate(debug=false)

        # Output the structure definition
        s = Structure.new
        s.config = self.config
        mod << s.generate(debug=debug)
        
        # Output the Allocated procedure
        a = Allocated.new
        a.config = self.config
        mod << a.generate(debug=debug)
        
        

      end
    end
  end
end
