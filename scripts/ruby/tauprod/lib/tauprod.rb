# Put tauprod heirarchy at
# beginning of search path
$:.unshift File.join(File.dirname(__FILE__))

require 'config/config'
require 'base/base'
require 'apodize/apodize'
#require 'convolve/convolve'

module TauProd
  class Processor
    def process(config)
      # Do the apodization
      pro = TauProd::Apodize::Processor.new(config)
      pro.process
      
      # Do the convolution
#      pro = TauProd::Convolve::Processor.new
#      pro.config = config
#      pro.process
    end
  end
end
