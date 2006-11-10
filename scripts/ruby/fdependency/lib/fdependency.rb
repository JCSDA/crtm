#!/usr/bin/ruby -w

require 'funit/fortran_deps'

# Redefine the Funit::Depend fortran_files_within method
class Funit::Depend
  def fortran_files_within( search_path = %w[ ../lib . ] )
    source = search_path.map{ |path| Dir[path+"/*.[fF]90"] }
    source.flatten!.uniq!
  end
end

# Output a dependency list
searchPath=["."]
fdepList=Funit::Depend.new(searchPath)
puts(fdepList.inspect)
ARGV.each do |file|
  puts fdepList.makefile_dependency_line(file)
end
