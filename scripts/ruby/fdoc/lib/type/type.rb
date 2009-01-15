module FDoc
  module Type
    class Generator < FDoc::Base
    
      def initialize(source)
        @source = source
      end
      
      def generate
        puts(self.class)
      end
      
    end
  end
end
