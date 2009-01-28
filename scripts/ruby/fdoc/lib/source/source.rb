module FDoc
  class Source

    # Constants
    ALLOWED_DELIMITER_TYPES = {:fdoc=>"fdoc", :xml=>"xml"}
    DEFAULT_DELIMITER_TYPE = ALLOWED_DELIMITER_TYPES[:fdoc]
    
    attr_accessor :file_name, :lines, :delimiter_type
    
    # Class constructor
    def initialize
      @file_name = ""
      @lines     = ""
      @delimiter = DEFAULT_DELIMITER_TYPE
    end
  
    # Class method to read a source file
    def self.load(file_name,delimiter_type=DEFAULT_DELIMITER_TYPE)
      source = self.new
      source.file_name      = file_name
      source.lines          = File.open(file_name,"r").readlines.join
      source.delimiter_type = delimiter_type
      source
    end
    
    # Instance method to extract code between delimited tags
    def extract(tag)

      # Define the documentation delimiters
      opening_delimiter = ""
      closing_delimiter = ""
      case
      when @delimiter_type == DEFAULT_DELIMITER_TYPE
        opening_delimiter = ":#{tag}\\+:"
        closing_delimiter = ":#{tag}-:"
      when @delimiter_type == ALLOWED_DELIMITER_TYPES[:xml]
        opening_delimiter = "<#{tag}>"
        closing_delimiter = "</#{tag}>"
      end

      # Build the regular expression
      regexp = %r{
                  ^\s*!\s*              # Whitespace before and after Fortran comment character
                  #{opening_delimiter}  # Opening delimiter
                    \n([\D\d]*?)\n      # Text between delimiters, excluding first and last newlines
                  ^\s*!\s*              # Whitespace before and after Fortran comment character
                  #{closing_delimiter}  # Closing delimiter
                 }ix

      # Extract out all the matches
      matches = @lines.scan(regexp).flatten
      
    end
    
  end
end
