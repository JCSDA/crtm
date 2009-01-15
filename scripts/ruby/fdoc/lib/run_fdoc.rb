#!/usr/bin/env ruby

require 'fdoc'

file_name = "test.f90"

# Get the source
# --------------
source = FDoc::Source.load(file_name)
puts(source.lines.class)

i1 = source.lines.index("  !:tdoc+\n")
i2 = source.lines.index("  !:tdoc-\n")
puts i1, i2
puts(source.lines[i1+1..i2-1])

# Begin the processing
# --------------------
FDoc::Generator.new.generate(source)
