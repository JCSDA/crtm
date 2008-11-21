# Put crtm_lib heirarchy at
# beginning of search path
$:.unshift File.join(File.dirname(__FILE__))

require 'config/config'
require 'base/base'
require 'build/build'
require 'link/link'

module CRTM_Lib
  class Processor
  
    def process(config)
      # Build the CRTM libraries
      if config.build
        pro = CRTM_Lib::Build::Builder.new(config)
        pro.build
      end

      # Link in the CRTM library
      if config.link
        pro = CRTM_Lib::Link::Linker.new(config)
        pro.link
      end
    end
    
  end
end
