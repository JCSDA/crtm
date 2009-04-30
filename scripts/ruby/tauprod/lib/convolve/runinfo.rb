require 'base/base'
module TauProd
  module Convolve
    class RunInfo < TauProd::Convolve::Base
      attr_accessor :root_dir,
                    :time

      def initialize(config)
        @config   = config
        @root_dir = Dir.pwd
        @time     = Time.now + config.start_delay
      end


      def setup
      
      end
      
    end  # RunInfo class
  end  # Convolve module
end  # TauProd module
