# Put svn_util heirarchy at
# beginning of search path
$:.unshift File.join(File.dirname(__FILE__))

module Svn_Util

  class Svn

    # The subversion actions associated
    # with particular file statuses
    ACTION = { "?" => "add",
               "!" => "remove" }
    
    # Make some of the instance variables visible
    attr_reader :url, :path
    
    # Public methods
    # --------------
    def initialize(url)
      @url    = url
      @path   = ""
      @status = {}
    end

    def checkout(path)
      @path = path
      system("svn checkout #{@url} #{@path}")
    end
    
    def commit(message)
      cmd = "svn commit -m \"#{message}\" #{@path}"
      system(cmd)
    end
    
    def status
      `svn status #{@path}`.split("\n").each do |line|
        @status[status_filename(line)] = status_type(line)
      end
    end

    def add
      status if @status.empty?
      action("?")
    end
    
    def delete
      status if @status.empty?
      action("!")
    end
    
  private

    # Private methods
    # ---------------
    def change(status_type)
      @status.select {|k,v| status_type.include?(v)}.transpose[0] || []
    end
    
    def action(status_type)
      action_type = ACTION[status_type]
      change(status_type).each do |f|
        system("svn #{action_type} #{f}")
      end if action_type
    end

    def status_type(line)
      line[0].chr
    end
    
    def status_filename(line)
      line[7..-1].chomp
    end
    
  end

end
