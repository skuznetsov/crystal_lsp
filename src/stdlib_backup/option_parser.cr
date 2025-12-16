# OptionParser - Command-line option parsing
# Minimal version for Crystal v2 bootstrap

class OptionParser
  @banner : String

  def initialize
    @banner = ""
  end

  # Set banner via method call
  def set_banner(text : String)
    @banner = text
  end

  # Get banner
  def banner : String
    @banner
  end
end
