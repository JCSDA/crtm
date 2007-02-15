require 'base/base'
module FGenMod
  module IO
    class Base < FGenMod::Base
      # This class is how the config gets passed along
      # and hold shared IO code

      # Method to determine the IO type
      # i.e. binary, netcdf, etc.
      def io_type
        self.module_name
      end

      # Method to determine the IO action
      # i.e. read, write, etc
      def io_action
        case self.class_name.downcase
          when "read" , "read_gatts", "inquire"  : "read"
          when "write", "write_gatts", "create"  : "write"
        end
      end

      # Method to determine the present
      # progressive of the IO action
      # i.e. reading, writing, or inquiring
      # for messages
      def io_ing
        case io_action
          when "read"   : "reading"
          when "write"  : "writing"
        end
      end

      # Method to determine the prepositions
      # to use with the io action in messages
      def io_tofrom
        io_action=="write" ? "to" : "from"
      end
      
      # Method to determine argument
      # intent based on class
      def io_intent
        case io_action
          when "read"  : "OUT"
          when "write" : "IN"
        end
      end

      # Method to determine argument
      # direction label based on class
      def io_dir
        case io_action
          when "read"  : "Output"
          when "write" : "Input"
        end
      end
    end
  end
end
