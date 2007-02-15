module FGenMod
  class Config
    require 'yaml'
    
    # ===============================
    # The fgenmod regular expressions
    # ===============================
    NAMESPACEREGEXP=%r{
        ^\s*NAMESPACE\s*:\s*
        (\w*)            # Capture the namespace prefix to $1
      }ix
      
    RELEASEREGEXP=%r{
        ^\s*RELEASE\s*:\s*
        (\w*)            # Capture the release number to $1
      }ix
      
    VERSIONREGEXP=%r{
        ^\s*VERSION\s*:\s*
        (\w*)            # Capture the version number to $1
      }ix
      
    STRUCTBEGINREGEXP=%r{
        ^\s*TYPE\s*::\s*
        (\w*)            # Capture the structure name to $1
      }ix

    STRUCTENDREGEXP=%r{
        ^\s*END\s+TYPE\s*
        (\w*)            # Capture the structure name to $1
      }ix

    DIMREGEXP=%r{
        ^\s*INTEGER\s*::\s* # Only integer types are allowed for dimensions
        (\w*)               # Capture the dimension name to $1
        # Look for description
        (?:                 # Non-capture grouping
          \s*!\s*           # Comment character + whitespace
          (.*)              # Capture the rest of the line to $2
        )?                  # The whole group is optional
      }ix
      
    COMPONENTREGEXP=%r{
        ^\s*
        # Look for the valid component type
        (REAL|INTEGER|COMPLEX|LOGICAL|CHARACTER)  # Capture the component type to $1
        # Look for the optional parameters or derived typename
        \s*
        (?:                             # Non-capture grouping
          \(                            # kind/len or type paren start
            \s*
            (?:(?:KIND|LEN)\s*=\s*)?    # Keywords are optional
            ([^()]\w+)                  # Capture parameter to $2
            \s*
          \)                            # kind/len or type paren end
        )?                              # The whole group is optional
        # Look for the dimension attribute
        (?:                             # Non-capture grouping
          \s*,\s*DIMENSION\s*           # attribute name
          \(                            # dimlist paren start
            ([-\w:,]+)                  # capture dimension list text to $3
          \)                            # dimlist paren end
        )?                              # The whole group is optional
        # Look for the component variable name
        \s*::\s*                        # The :: separator
        (\w*)                           # Capture the component variable name to $4
        # Look for init val
        (?:                             # Non-capture grouping
          \s*=\s*                       # Assignment operator + whitespace
          (['"'.,()\s\w]*)              # Capture the initialisation value or parameter name to $5
        )?                              # The whole group is optional
        # Look for description
        (?:                             # Non-capture grouping
          \s*!\s*                       # Comment character + whitespace
          (.*)                          # Capture the rest of the line to $6
        )?                              # The whole group is optional
      }ix
    
    # =====================
    # Class variable access
    # =====================
    attr_accessor :namespace                          # The name space prefix
    attr_accessor :struct_name                        # The structure name
    attr_accessor :release,:version                   # The structure/file release and verison number
    attr_accessor :dim_list,:scalar_list,:array_list  # The structure component lists
    attr_accessor :gatts                              # The netCDF global attribute strings
    attr_accessor :dimdecl                            # The local dimension loop variables
    attr_accessor :debug                              # Have a guess

    # =================
    # Class constructor
    # =================
    def initialize(debug=false)
      @namespace  =""
      @struct_name=""
      @release    =nil
      @version    =nil
      @dim_list   =[]
      @scalar_list=[]
      @array_list =[]
      @gatts      =[]
      @debug      =debug
      @dimdecl    =""
    end

    # ========================================
    # Class method to load fgenmod definitions
    # ========================================
    def self.load(file_name,debug=false)
      config=new(debug)
      config.parse(File.open(file_name,"r").readlines)
      config
    end
    
    # ===================================
    # Method to parse fgenmod definitions
    # ===================================
    def parse(lines)

      # Set the component identifier tag
      tag=""
      
      # Parse line by line
      lines.each do |line|

        # Retrieve the namespace prefix
        if line =~ NAMESPACEREGEXP
          @namespace=$1+"_"
          puts("\nNamespace prefix: #{@namespace}") if @debug
          next
        end
      
        # Retrieve the release number
        if line =~ RELEASEREGEXP
          @release=$1
          puts("\nRelease number: #{@release}") if @debug
          next
        end
      
        # Retrieve the version number
        if line =~ VERSIONREGEXP
          @version=$1
          puts("\nVersion number: #{@version}") if @debug
          next
        end
      
        # Retrieve the structure name
        if line =~ STRUCTBEGINREGEXP
          @struct_name=$1
          puts("\nStructure name: #{@struct_name}") if @debug
          next
        end
      
        # Check for definition end
        break if line =~ STRUCTENDREGEXP
      
        # Match fgenmod tags and set switches
        case line
        when /^\s*#\s*Dimensions/i
          tag="dim"
          puts("\nDimensions") if @debug
          next
        when /^\s*#\s*Scalars/i
          tag="scalar"
          puts("\nScalar components") if @debug
          next
        when /^\s*#\s*Arrays/i
          tag="array"
          puts("\nArray components") if @debug
          next
        end
      
        # Skip comments
        next if line =~ /^\s*!.*/
        
        # Process structure components
        case tag
        when "dim"
          dim_process(line)
          next
        when "scalar"
          scalar_process(line)
          next
        when "array"
          array_process(line)
          next
        end
        
      end

      # Check that all array component dimensions are valid
      dim_check()
      
      self
    end

    # ========================================
    # Method to process a dimension definition
    # ========================================
    def dim_process(line)
      dim_match = COMPONENTREGEXP.match(line)
      unless dim_match.nil? || (dim_match[1].upcase != "INTEGER")
        # We have matched a INTEGER component definition
        @dim_list<<dim_match[4]
        puts(@dim_list.last.to_yaml) if @debug
      else
        # No match, so raise an error
        raise StandardError, "Invalid dimension definition: #{line}"
      end
    end 

    # =====================================
    # Method to process a scalar definition
    # =====================================
    def scalar_process(line)
      scalar_match = COMPONENTREGEXP.match(line)
      unless scalar_match.nil?
        # We have matched a scalar component definition
        @scalar_list<<{:type     => scalar_match[1].upcase,
                       :param    => scalar_match[2],
                       :name     => scalar_match[4],
                       :initval  =>(scalar_match[5].chomp.rstrip unless scalar_match[5].nil?),
                       :desc     => scalar_match[6]}
        puts(@scalar_list.last.to_yaml) if @debug
      else
        # No match, so raise an error
        raise StandardError, "Invalid scalar definition"
      end
    end

    # =====================================
    # Method to process an array definition
    # =====================================
    def array_process(line)
      array_match = COMPONENTREGEXP.match(line)
      unless array_match.nil?
        # We have matched an array component definition
        dims=dim_extents(array_match[3])   # Array of dimension extents
        ndims=dims.length                  # The number of dimensions
        dimidx=dim_idxvar(ndims)           # Loop index variables for each dimension
        @array_list<<{:type     => array_match[1].upcase,
                      :param    => array_match[2],
                      :dims     => dims,
                      :ndims    => ndims,
                      :dimidx   => dimidx,
                      :name     => array_match[4],
                      :desc     => array_match[6]}
        @dimdecl=dimidx if ndims>@dimdecl.length             
        puts(@array_list.last.to_yaml) if @debug
      else
        # No match, so raise an error
        raise StandardError, "Invalid array definition"
      end
    end

    # Helper methods
    def dim_extents(dim_list)
      dims=[]
      dim_list.split(/\s*,\s*/).each do |d|
        thisdim=d.split(/\s*:\s*/)
        thisdim=["1"]+thisdim if thisdim.length == 1
        dims<<thisdim
      end
      dims
    end

    def dim_idxvar(ndims)
      Array.new(ndims) {|i| "i"+(i+1).to_s}
    end
      
    # ==============================
    # Method to check the dimensions
    # ==============================
    def dim_check
      @array_list.each do |a|
        a[:dims].each do |d|
          dim=/[_a-zA-Z0-9]+/.match(d[1])[0]
          raise(StandardError, "Invalid dimension, #{dim}, specified for #{a[:name]}") unless @dim_list.include?(dim)
        end
      end
    end

  end
end
