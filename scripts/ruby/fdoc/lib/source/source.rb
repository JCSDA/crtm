module FDoc
  class Source
  
    attr_accessor :file_name, :lines
    
    # Class constructor
    def initialize
      @file_name = ""
      @lines     = ""
    end
  
    # Class method to read a source file
    def self.load(file_name)
      source = self.new
      source.file_name = file_name
      source.lines = File.open(file_name,"r").readlines.join
      source
    end
    
    # Instance method to extract code between a delimiter
    def extract(delimiter)

#regexp = Regexp.new("#{delimiter}\\+(\\s*|\\w*)*#{delimiter}-")
regexp = Regexp.new("\\s*!\\s*#{delimiter}[^+]*\\+((?:\\s*\\w*\\s*)*?)\\s*!\\s*#{delimiter}-")
puts regexp.inspect
x = @lines.scan(regexp)
puts x.inspect
x
#if @lines =~ regexp
#  puts $1
#end
#
#      extracted_string = []
#      lines = @lines
#      i1 = lines.index("#{delimiter}+")
#      i2 = lines.index("#{delimiter}-")
#      extracted_string << lines[i1,i2-i1].chomp.to_a[1..-2].join
    end
    
  end
end
