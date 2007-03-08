require 'config/config'
#require 'tape5/tape5'
#require 'tauspc/tauspc'
#require 'tauprofile/tauprofile'

module TauProd

  # Main processor of TauProd
  class Processor

    # Processors for individual parts
    # Just add another class to run
    # another part
    PROCESSORS = [
    ]
#      TauProd::TAPE5::Processor,
#      TauProd::TauSpc::Processor,
#      TauProd::TauProfile::Processor
#    ]

    # Main process method
    def process(config_file_name)
      config = Config.load(config_file_name)
      PROCESSORS.each do |pro_class|
        pro = pro_class.new
        pro.config = config
        pro.process
      end
    end
  end
end
