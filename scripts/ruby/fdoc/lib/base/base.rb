module FDoc

  # FDoc base class with shared code
  class Base

    def signal(file_name)
      puts("#{self.class}: Created #{file_name}")
    end
  
  end
end
