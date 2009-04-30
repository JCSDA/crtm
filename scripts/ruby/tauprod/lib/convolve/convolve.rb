require 'convolve/base'
require 'convolve/calculator.rb'
require 'convolve/convolver.rb'
require 'convolve/assembler.rb'

module TauProd
  module Convolve
    class Processor < TauProd::Convolve::Base
      # Processors for individual parts
      # Just add another class to run
      # another part
      PROCESSORS = [ Calculator ] #, Convolver, Assembler ]

      # Method to process the convolutions
      def process
        puts self.class
        PROCESSORS.each do |pro_class|
          pro = pro_class.new(self.config)
          pro.process
        end
      end  # process method

    end  # Processor class
  end  # Apodize module
end  # TauProd module
