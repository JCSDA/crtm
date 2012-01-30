require 'define/base'
require 'define/structure'
require 'define/allocated'
require 'define/destroy'
require 'define/create'
require 'define/inspect'

module SrcGen
  module Define
  
    class Generator < SrcGen::Define::Base
      
      # Method to generate the structure definition
      def generate(debug=false)

        mod = ""
        
        # Output the structure definition
        s = Structure.new
        s.config = self.config
        mod << s.generate(debug=debug)
        
        # Output the allocation check procedure
        a = Allocated.new
        a.config = self.config
        mod << a.generate(debug=debug)
        
        # Output the destruction procedure
        d = Destroy.new
        d.config = self.config
        mod << d.generate(debug=debug)
        
        # Output the creation procedure
        c = Create.new
        c.config = self.config
        mod << c.generate(debug=debug)
        
        puts mod

      end
    end
  end
end
