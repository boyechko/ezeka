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
  def self.dir(symbol)
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

  # Returns true if the given string names a valid kasten
  def self.valid_kasten?(string)
    return ['main', 'limbo', 'tech', 'writing'].include?(string)
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

  # Generates a YAML block as a string, using inline sequence style. Can't use
  # YAML::to_yaml() because it does not support inline style.
  def self.to_yaml(hash)
    result = ""
    hash.each do |key, val|
      if val.is_a?(Array)
        result += "#{key}: [ #{val.join(', ')} ]\n"
      else
        result += "#{key}: #{val}\n"
      end
    end
    return result
  end
end

class Numerus < Zettel
  SLUG_PATTERN = /^([0-9]{3})(-([a-z]+))*$/

  @type = :numerus_currens
  @kasten = :main
  attr_reader :number, :letters # for numerus currens

  def initialize(string)
    super(name)
    # Figure out of we're passed a link or a path
    if File.exists?(string) then
      path = Pathname(string)
      @name = path.basename(Zettelkasten.ext).to_s
    elsif string =~ SLUG_PATTERN
      @name = string
      @number = $1.to_i
      @letters = $3
    else
      raise TypeError, "Invalid name for a numerus currens Zettel: '#{name}'"
    end

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
  end

  # Returns the wiki link target
  def link_target()
    return @name
  end

  # Returns true if the link (a string) is a valid link to numerus currens zettel
  def self.valid_link?(link)
    return link =~ SLUG_PATTERN ? true : false
  end

  # Returns true if the string is a valid link or path a to numerus currens zettel
  def self.valid_link_or_path?(string)
    return string =~ SLUG_PATTERN ||
           File.basename(string, Zettelkasten.ext) =~ SLUG_PATTERN ? true : false
  end
end

class Tempus < Zettel
  LINK_PATTERN = /^([a-z]+):(\d{8}T\d{4})$/
  SLUG_PATTERN = /^\d{8}T\d{4}$/

  @type = :iso8601
  attr_reader :time

  def initialize(string)
    # Figure out of we're passed a slug or a path
    if File.exists?(string) && valid_link_or_path?(string) then
      # FIXME: This would allow paths outside of the Zettelkasten
      path = Pathname(string)
      @kasten = path.dirname.to_s.to_sym
      @name = path.basename(Zettelkasten.ext).to_s
    elsif string =~ LINK_PATTERN
      if Zettelkasten.valid_kasten?($1.to_sym)
        @kasten = $1.to_sym
        @name = $2
      else
        raise TypeError, "Unknown Kasten: '#{$1.to_sym}'"
      end
    else
      raise TypeError, "Invalid name for a tempus Zettel: '#{name}'"
    end
    super(name)

    @time = Time.parse(name)
    @directory = "#{Zettelkasten.dir(@kasten)}"
    @path = "#{@directory}/#{@name}#{Zettelkasten.ext}"
    initialize_metadata
  end

  # Returns the wiki link target
  def link_target()
    return "#{@kasten.to_s}:#{@name}"
  end

  # Returns true if the link (a string) is a valid link to tempus zettel
  def self.valid_link?(link)
    return link =~ LINK_PATTERN ? true : false
  end

  # Returns true if the string is a valid link or path a to numerus currens zettel
  def self.valid_link_or_path?(string)
    if string =~ LINK_PATTERN
      return true
    else
      path = Pathname(string)
      if path.basename(Zettelkasten.ext).to_s =~ SLUG_PATTERN &&
         Zettelkasten.valid_kasten?(path.dirname.split[1].to_s)
        return true
      else
        return false
      end
    end
  end
end
