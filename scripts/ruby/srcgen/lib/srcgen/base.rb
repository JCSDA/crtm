module SrcGen
  class Base
    attr_accessor :config
    
    # ======================================
    # BEGIN: String array processing methods
    
    public
        
    # ...Pad a string array with spaces to yield same length strings
    def pad_string(array)
      input = to_array(array)
      padded_array = []
      max_length = input.collect {|e| e.length}.max
      input.each {|e| padded_array << e + " "*(max_length - e.length)}
      padded_array
    end

    # ...Prepend a string to each element of a string array, or
    # ...prepend corresponding elements of one string array to another.
    def prepend_string(main_string,edge_string,pad=false)
      main_array, edge_array = process_arguments(main_string,edge_string,pad=pad)
      attach_array(main_array, edge_array, prepend=true)
    end

    # ...Append a string to each element of a string array, or
    # ...append corresponding elements of one string array to another.
    def append_string(main_string,edge_string,pad=false)
      main_array, edge_array = process_arguments(main_string,edge_string,pad=pad)
      attach_array(main_array, edge_array)
    end


    # ...Sandwich each element of a string array with strings.
    def sandwich_string(prepend,array,append,last=false)
      prepended_array = prepend_string(array,prepend,pad=true)
      if last
        appended_array = append_string(prepended_array[0..-2],append)
        appended_array << append_string(prepended_array[-1],last)
      else
        appended_array = append_string(prepended_array,append)
      end
      appended_array.flatten
    end

    private
  
    def to_array(input)
      [input].flatten
    end

    def match_array(main_array,edge_array)
      Array.new(main_array.size,edge_array[0])
    end

    def process_arguments(main_string,edge_string,pad)
      main_array, edge_array = to_array(main_string), to_array(edge_string)
      main_array = pad_string(main_array) if pad
      edge_array = match_array(main_array,edge_array) unless edge_array.length == main_array.length
      return main_array, edge_array
    end

    def attach_array(main_array, edge_array, prepend=false)
      output = []
      main_array.each_with_index {|e,i| output << (prepend ? "#{edge_array[i]}#{e}" : "#{e}#{edge_array[i]}")}
      output
    end
    
    # END: String array processing methods
    # ====================================
    
  
  end
end
