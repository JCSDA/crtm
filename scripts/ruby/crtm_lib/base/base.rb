require 'fileutils'

# CRTM_Lib base class with all shared bits of code.
module CRTM_Lib
  class Base
    attr_accessor :config

    # Method to check if the various directories exist
    def install_dir?
      File.directory?(@config.install_dir)
    end
    def lib_dir?
      File.directory?(@config.lib_dir)
    end
    def include_dir?(target="")
      include_dir = @config.include_dir + (target.empty? ? "" : "_#{target}")
      File.directory?(include_dir)
    end

    # Method to create the various directories if they don't exist
    def create_lib_dir
      create_dir(@config.lib_dir)
    end
    def create_include_dir(target="")
      include_dir = @config.include_dir + (target.empty? ? "" : "_#{target}")
      create_dir(include_dir)
    end
    
  private
  
    # Method to create a directory if it doesn't exist
    def create_dir(dir)
      FileUtils.mkdir(dir) unless File.directory?(dir)
    end
    
  end
end
