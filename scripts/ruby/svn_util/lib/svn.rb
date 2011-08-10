# Put svn_util heirarchy at
# beginning of search path
$:.unshift File.join(File.dirname(__FILE__))

module Svn_Util

  require 'xmlsimple'
  require 'yaml'

  class Svn

    # The subversion actions associated
    # with particular file statuses
    ACTION = { "?" => "add",
               "!" => "remove" }
    
    # Make some of the instance variables visible
    attr_reader :url, :path
    
    # Public methods
    # --------------
    def initialize()
      @url    = nil
      @path   = nil
      @info   = nil
      @status = {}
      @list   = {}
    end


    def checkout(url,path="",options={})
      ignore_externals = options[:ignore_externals] ? "--ignore-externals " : ""
      quiet            = options[:quiet]            ? "--quiet " : ""
      depth            = options[:depth]            ? "--depth #{options[:depth]} " : ""
      revision         = options[:revision]         ? "--revision #{options[:revision]} " : ""
      @url = url
      cmd = "svn checkout #{ignore_externals}#{quiet}#{depth}#{revision} #{url} #{path}"
      system(cmd)
    end
    
    
    def commit(path,options={})
      if options[:message] then
        cmd = "svn commit --message \"#{options[:message]}\" #{path}"
      end
      if options[:file] then
        cmd = "svn commit --file #{options[:message]} #{path}"
      end
      system(cmd)
    end
    
    
    def copy(source,destination,options={})
      peg_revision     = options[:peg_revision]     ? "@#{options[:peg_revision]}" : ""
      file             = options[:file]             ? "--file #{options[:file]} " : ""
      force_log        = options[:force_log]        ? "--force-log " : ""
      ignore_externals = options[:ignore_externals] ? "--ignore-externals " : ""
      message          = options[:message]          ? "--message #{options[:message]} " : ""
      parents          = options[:parents]          ? "--parents " : ""
      quiet            = options[:quiet]            ? "--quiet " : ""
      revision         = options[:revision]         ? "--revision #{options[:revision]} " : ""
      with_revprop     = options[:with_revprop]     ? "--with-revprop #{options[:with_revprop]} " : ""
      cmd = "svn copy #{file}#{force_log}#{ignore_externals}#{message}#{parents}#{quiet}#{revision}#{with_revprop} #{source}#{peg_revision} #{destination}"
      system(cmd)
    end
    
    
    def mkdir(directory,options={})
      file             = options[:file]             ? "--file #{options[:file]} " : ""
      force_log        = options[:force_log]        ? "--force-log " : ""
      message          = options[:message]          ? "--message #{options[:message]} " : ""
      parents          = options[:parents]          ? "--parents " : ""
      quiet            = options[:quiet]            ? "--quiet " : ""
      with_revprop     = options[:with_revprop]     ? "--with-revprop #{options[:with_revprop]} " : ""
      cmd = "svn mkdir #{file}#{force_log}#{message}#{parents}#{quiet}#{with_revprop} #{directory}"
      system(cmd)
    end
    
    
    def status(path="",options={})
      changelist       = options[:changelist]       ? "--changelist #{options[:changelist]} " : ""
      depth            = options[:depth]            ? "--depth #{options[:depth]} " : ""
      ignore_externals = options[:ignore_externals] ? "--ignore-externals " : ""
      incremental      = options[:incremental]      ? "--incremental " : ""
      no_ignore        = options[:no_ignore]        ? "--no-ignore " : ""
      quiet            = options[:quiet]            ? "--quiet " : ""
      show_updates     = options[:show_updates]     ? "--show-updates " : ""
      verbose          = options[:verbose]          ? "--verbose " : ""
      debug            = options[:debug]
      cmd = "svn status --xml #{changelist}#{depth}#{ignore_externals}#{incremental}#{no_ignore}#{quiet}#{show_updates}#{verbose} #{path}"
      @status = XmlSimple.xml_in(`#{cmd}`)["target"][0]
      if debug
        puts(@status.to_yaml)
        puts(@status.inspect)
      end
#      @status["entry"].each do |e|
#        e.each_pair {|k,v| puts "#{k.inspect} maps to #{v}"} 
#      end
#      `svn status #{@path}`.split("\n").each do |line|
#        @status[status_filename(line)] = status_type(line)
#      end
    end


    def add
      status if @status.empty?
      action("?")
    end
    
    
    def delete
      status if @status.empty?
      action("!")
    end
    
    
    def info(target="",options={})
      changelist  = options[:changelist]  ? "--changelist #{options[:changelist]} " : ""
      depth       = options[:depth]       ? "--depth #{options[:depth]} " : ""
      incremental = options[:incremental] ? "--incremental " : ""
      recursive   = options[:recursive]   ? "--recursive " : ""
      revision    = options[:revision]    ? "--revision #{options[:revision]} " : ""
      targets     = options[:targets]     ? "--targets #{options[:targets]} " : ""
      debug       = options[:debug]
      cmd = "svn info --xml #{changelist}#{depth}#{incremental}#{recursive}#{revision}#{targets} #{target}"
      @info = XmlSimple.xml_in(`#{cmd}`)["entry"][0]
      if debug
        puts(@info.to_yaml)
        puts(@info.inspect)
        @info.each_pair {|k,v| puts "#{k.inspect} maps to #{v}"} 
      end
    end
    
    
    def list(target="",options={})
      depth       = options[:depth]       ? "--depth #{options[:depth]} " : ""
      incremental = options[:incremental] ? "--incremental " : ""
      recursive   = options[:recursive]   ? "--recursive " : ""
      revision    = options[:revision]    ? "--revision #{options[:revision]} " : ""
      verbose     = options[:verbose]     ? "--verbose " : ""
      debug       = options[:debug]
      cmd = "svn list --xml #{depth}#{incremental}#{recursive}#{revision}#{verbose} #{target}"
      @list = XmlSimple.xml_in(`#{cmd}`)["list"][0]
      if debug
        puts(@list.to_yaml)
        puts(@list.inspect)
      end
    end
    
    
    def versioned?(path,options={})
      debug = options[:debug]
      return false unless File.exist?(path)
      status(path=path, :depth => "immediates", :debug => debug)
      return true if @status["entry"].nil?
      is_versioned = false
      @status["entry"].each do |e|
        next unless e["path"] == path
        wc_status = e["wc-status"][0]
        puts(wc_status.inspect) if debug
        is_versioned = (wc_status["item"] != "unversioned")
        break
      end
      is_versioned
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


    def parse_options(options={})
      return if options.empty?
    end
    
  end

end
