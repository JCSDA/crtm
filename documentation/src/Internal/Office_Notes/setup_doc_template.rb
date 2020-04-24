#!/usr/bin/env ruby

# Script to create template document directories.
#
# $Id$

require 'yaml'
require 'fileutils'

info_file = ".author.info"
info = { "AUTHOR" => "AUTHOR",
         "EMAIL"  => "EMAIL",
         "AFFILIATION" => "AFFILIATION" }
info = YAML.load_file(info_file) if File.exists?(info_file)

date = Time.now
info["MONTH"] = date.strftime("%B")
info["YEAR"] = date.strftime("%Y")
info["YYYY-MM-DD"] = date.strftime("%Y-%m-%d")

if ARGV.empty?
  puts("ERROR: Must specify a document directory name")
  exit 1
end

ARGV.each do |dir|
  if File.exists?(dir)
    puts("Document directory #{dir} already exists. Skipping...")
    next
  end
  FileUtils.mkdir(dir)
  FileUtils.chdir(dir) do
    FileUtils.ln_s("../bibliography.bib",".")
    FileUtils.ln_s("../preamble.tex",".")
    tex_file = "#{dir}.tex"

    doc = File.readlines("../template.tex")
    info.each do |key,value|
      re = Regexp.new(key)
      doc.find_all {|s| s=~re}.each {|n| n.gsub!(re,value)}  
    end
    File.open(tex_file,"w") {|f| f.puts(doc)}
  
    File.open("Makefile","w") do |f|
      f.puts("DOC_FILE = #{tex_file}")
      f.puts("include ../make.common")
    end
  end
end
