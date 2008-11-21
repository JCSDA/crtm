#!/usr/bin/env ruby

require 'crtm_lib'


# Configuration setup
# -------------------
config = CRTM_Lib::Config.new()
config.process_arguments(ARGV)
config.display


# Begin the processing
# --------------------
CRTM_Lib::Processor.new.process(config)

