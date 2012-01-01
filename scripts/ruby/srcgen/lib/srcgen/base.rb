module SrcGen
  class Base
    attr_accessor :config
    
    def pad_string(array)
      padded_array = []
      max_length = array.collect {|e| e.length}.max
      array.each {|e| padded_array << e + " "*(max_length - e.length)}
      padded_array
    end

    def prepend_string(array,string)
      array.collect {|e| "#{string}#{e}"}
    end    

    def append_string(array,string)
      array.collect {|e| "#{e}#{string}"}
    end    

    def prepend_pad_append_string(prepend,array,append,last=false)
      prepended_array = prepend_string(array,prepend)
      padded_array = pad_string(prepended_array)
      if last
        appended_array = append_string(padded_array[0..-2],append)
        appended_array << append_string(padded_array[-1],last)
      else
        appended_array = append_string(padded_array,append)
      end
      appended_array
    end
  end
end
