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
#      # XML-style delimiters:  <tag> and </tag>    
#      regexp = %r{
#                  ^\s*!\s*        # Whitespace before and after Fortran comment character
#                  <#{delimiter}>  # Opening delimiter
#                    ([\D\d]*?)    # Text between delimiters
#                  ^\s*!\s*        # Whitespace before and after Fortran comment character
#                  </#{delimiter}> # Closing delimiter
#                 }ix

      # CRTM-style delimiter:  :tag+  and  :tag-    
      regexp = %r{
                  ^\s*!\s*          # Whitespace before and after Fortran comment character
                  :#{delimiter}\+   # Opening delimiter
                    \n([\D\d]*?)\n  # Text between delimiters, excluding first and last newlines
                  ^\s*!\s*          # Whitespace before and after Fortran comment character
                  :#{delimiter}-    # Closing delimiter
                 }ix
                 
      # Extract out all the matches
      matches = @lines.scan(regexp).flatten
    end
    
  end
end
