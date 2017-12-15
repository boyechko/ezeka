# -----------------------------------------------------------------------------
#      Author: Roderick Hoybach <hoybach-code@diachronic.net>
# Description: A collection of methods for working with Zettel
#     Created: 2016-09-02
#    Comments:
# -----------------------------------------------------------------------------

require 'time'
require 'yaml'

class Zettelkasten
  class << self; attr_accessor :root end
  class << self; attr_accessor :main end
  class << self; attr_accessor :limbo end
  class << self; attr_accessor :tech end
  class << self; attr_accessor :writing end
  class << self; attr_accessor :ext end

  @root = ENV['ZETTEL_DIR'] || File.expand_path('~/Dropbox/Zettel')
  @main = "#{@root}/main"
  @limbo = "#{@root}/limbo"
  @tech = "#{@root}/tech"
  @writing = "#{@root}/writing"

  # Default extension for Zettelkasten files
  @ext = ".txt"

  # Returns the directory for the kasten as a symbol
  def Zettelkasten.dir(symbol)
    case symbol
    when :main
      @main
    when :limbo
      @limbo
    when :tech
      @tech
    when :writing
      @writing
    else
      raise "Unknown kasten '#{symbol}'"
    end
  end
end

class Zettel
  attr_reader :type, :name, :kasten, :directory, :path
  attr_accessor :metadata

  def initialize(name)
    @name = name
  end

  def initialize_metadata()
    if File.exists?(@path)
      begin
        @metadata = YAML::load(File.read(path).split("\n\n", 2)[0])
      rescue Exception => e
        raise "Malformed metadata: #{e.message}"
      end
    end
  end

  # Returns true if the zettel exists where it should
  def exists?
    File.exists?(@path)
  end
end

class Numerus < Zettel
  @type = :numerus_currens
  @kasten = :main
  attr_reader :number, :letters # for numerus currens

  def initialize(name)
    super(name)

    if name =~ /^([0-9]{3})(-([a-z]+))*$/
      @number = $1.to_i
      @letters = $3

      # Set the directory
      if @number >= 0 and @number <= 99
        @directory = "#{Zettelkasten.main}/000-099"
      elsif @number >= 100 and @number <= 999
        @directory = "#{Zettelkasten.main}/#{@name[0]}00-#{@name[0]}99"
      else
        raise "Numerus currens '#{name}' is out of bounds (0-999)"
      end
      @path = "#{@directory}/#{@name}#{Zettelkasten.ext}"
      initialize_metadata
    else
      raise TypeError, "Invalid name for a numerus currens Zettel: '#{name}'"
    end
  end
end

class Tempus < Zettel
  @type = :iso8601
  attr_reader :time

  def initialize(name)
    super(name)

    if name =~ /^([a-z]+):(\d{8}T\d{4})$/
      @kasten = $1.to_sym
      @name = $2
      @time = Time.parse(name)
      @directory = "#{Zettelkasten.dir(@kasten)}"
      @path = "#{@directory}/#{@name}#{Zettelkasten.ext}"
      initialize_metadata
    else
      raise TypeError, "Invalid name for a tempus Zettel: '#{name}'"
    end
  end
end
