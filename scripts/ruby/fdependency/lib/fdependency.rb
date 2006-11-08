#!/usr/bin/env ruby

require 'funit/fortran_deps'

x=Funit::Depend.new
ARGV.each do |file|
  puts(x.makefile_dependency_line(file))
end

puts("got this far!")
