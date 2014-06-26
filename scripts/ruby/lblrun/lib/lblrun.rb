# Put crtm_lib heirarchy at
# beginning of search path
$:.unshift File.join(File.dirname(__FILE__))

require 'config/config'

module LBLrun
  class Processor
  
    def process(config)
      puts("stub!")
    end
    
  end
end
