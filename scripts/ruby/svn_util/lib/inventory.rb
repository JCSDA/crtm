# Put svn_util heirarchy at
# beginning of search path
$:.unshift File.join(File.dirname(__FILE__))

module Svn_Util

  class Inventory
    require 'fileutils'

    # The files/directories listed below will
    # not be included in the process  
    EXCLUDE_NAMES = ["CVS"]
    

    # Inventoring methods
    # -------------------
    def initialize(filename)
      unless File.file?(filename)
        inventory()
        write_inventory(filename)    
      end
      read_inventory(filename)
      inventory()
      @filename = filename
    end

    def added
      @current - @previous
    end

    def deleted
      @previous - @current
    end

    def exclude?(line)
      EXCLUDE_NAMES.any? {|e| line.split("/").include?(e)}
    end
    
    def get_inventory
      Dir.glob('**/*').sort.reject {|line| exclude?(line)}
    end

    def inventory
      @current = get_inventory()
    end
    
    def write_inventory(filename)
      File.open(filename,'w') do |f|
        @current.each {|line| f << "#{line}\n"}
      end
    end

    def read_inventory(filename)
      @previous = File.open(filename).collect do |line|
        line.chomp
      end
      @previous.reject {|line| exclude?(line)}
    end

    def print_files(files)
      files.each {|f| puts("  #{f}")}
    end
    
    def ireport(action)
      files = method(action).call
      unless files.empty?
        puts("The following files have been #{action}:")
        print_files(files)
      end
    end

    def report
      ireport(:added)
      ireport(:deleted)
    end


    # Directory manipulation methods
    # ------------------------------
    def dirs
      @current.select {|f| File.directory?(f)}
    end
      
    def empty_dirs
      dirs.select {|d| Dir["#{d}/*"].empty?}
    end
    
    def non_empty_dirs
      dirs - empty_dirs
    end
    
    def remove_empty_dirs
      puts("Removing empty directories:") unless empty_dirs.empty?
      until empty_dirs.empty?
        empty_dirs.each do |d|
          puts("  #{d}")
          @current.delete(d)
          FileUtils.rm_rf(d)
        end
      end
    end
    
    
    # Working copy rsync methods
    # --------------------------
    def rsync_dirs(from, to)
      FileUtils.cd(from) do
        cmd = "rsync -avz --delete --force --exclude .svn --exclude #{@filename} . #{to}"
        system(cmd)
      end
    end
    
  end

end
