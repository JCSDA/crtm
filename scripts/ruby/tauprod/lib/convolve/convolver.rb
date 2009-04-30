require 'convolve/base'
module TauProd
  module Convolve
    class Convolver < TauProd::Convolve::Base
      def process
        puts self.class
      end  # process method
    end  # Convolver class
  end  # Convolve module
end  # TauProd module

