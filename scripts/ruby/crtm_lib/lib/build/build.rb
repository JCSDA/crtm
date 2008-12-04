require 'fileutils'

module CRTM_Lib
  module Build
    class Builder < CRTM_Lib::Base
    
      def initialize(config)
        @config = config
      end
      
      def build
        # Check the install directory
        unless install_dir?
          puts("\nERROR: #{@config.install_dir} doesn't exist. I'm not going to create it!")
          exit 1
        end
        
        # Create the library directory
        create_lib_dir
        
        # Build the bits
        FileUtils.chdir(@config.crtm_dir,:verbose=>@config.debug) do
          run("make create_links CRTM_SOURCE_ROOT=#{@config.crtm_dir}")
          FileUtils.chdir("Build",:verbose=>@config.debug) do
            @config.targets.each do |t|
              # Create the include dir
              create_include_dir(t)
              # Build the library and install
              run("make clean")
              run("make #{t}")
              run("make install")
              run("mv include/*.mod #{@config.include_dir}_#{t}")
              run("mv lib/libCRTM.a #{@config.lib_dir}/libCRTM.a.#{t}")
            end
          end
        end
      end
      
    private
    
      # Method to either output or execute system commands
      def run(cmd)
        if @config.debug then
          puts(cmd)
        else
          system(cmd)
        end
      end
      
    end
  end
end
