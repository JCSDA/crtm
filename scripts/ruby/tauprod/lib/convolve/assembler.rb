require 'convolve/base'
module TauProd
  module Convolve
    class Assembler < TauProd::Convolve::Base
      def process
        puts self.class
      end  # process method
    end  # Assembler class
  end  # Convolve module
end  # TauProd module

