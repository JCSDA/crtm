module TauProd

  # TauProd base class with all shared bits of code.
  class Base
    attr_accessor :config

    # Method to replace only the first
    # occurance of the leading spaces
    # in each line of input text.
    def strip_output(text)
      text =~ /^\s+/
      leading_spaces = $&
      text = text.to_a.collect {|l| l.sub(/^#{leading_spaces}/,"")}.to_s
    end
  end
end
