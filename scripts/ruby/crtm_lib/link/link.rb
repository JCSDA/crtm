require 'fileutils'

module CRTM_Lib
  module Link
    class Linker < CRTM_Lib::Base
    
      def initialize(config)
        @config = config
      end
      
      def link
        # Only link in the last of the specified targets
        t = @config.targets.last
        include_dir = File.basename("#{@config.include_dir}_#{t}")
        lib_file = "libCRTM.a"
        
        FileUtils.chdir(@config.install_dir, :verbose=>@config.debug) do
          # Link in the required include directory
          FileUtils.rm_f("include", :verbose=>@config.debug)
          if include_dir?(target=t) then
            FileUtils.ln_sf(include_dir, "include", :verbose=>@config.debug)
          end
          # Link in the particular library
          FileUtils.chdir("lib", :verbose=>@config.debug) do
            FileUtils.rm_f(@config.lib_file, :verbose=>@config.debug)
            if File.exists?("#{@config.lib_file}.#{t}") then
              FileUtils.ln_sf("#{@config.lib_file}.#{t}", @config.lib_file, :verbose=>@config.debug)
            end
          end
        end
      end
      
    end
  end
end
