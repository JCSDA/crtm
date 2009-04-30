require 'apodize/base'
require 'apodize/runinfo'

module TauProd
  module Apodize
    class Processor < TauProd::Apodize::Base

      def initialize(config)
        @config  = config
        @runinfo = RunInfo.new(config)
      end
      
      def process
        puts self.class
      
        # Begin the error handling block
        # ------------------------------
        begin

          # Begin iteration over cases
          # --------------------------
          @config.bands.each do |b|
            @config.molids.each do |m|
              @config.profiles.each do |p|
                
                @runinfo.setup(b,m,p)
                @config.angles.each {|a| @runinfo.angle_setup(a)}
                @runinfo.submit

              end # profile loop

              @runinfo.increment_time

            end # molecule loop
          end # band loop
           
        rescue StandardError => error_message
          puts("\nERROR: #{error_message}\n")
          exit 1
        end  # error handling block
        
      end  # process method
    end  # Processor class
  end  # Apodize module
end  # TauProd module
