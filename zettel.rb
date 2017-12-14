# -----------------------------------------------------------------------------
#      Author: Roderick Hoybach <hoybach-code@diachronic.net>
# Description: A collection of methods for working with Zettel
#     Created: 2016-09-02
#    Comments:
# -----------------------------------------------------------------------------

require 'time'

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

class Metadata
  attr_accessor :hash
  attr_reader :title, :subtitle, :created, :modified, :oldname,
              :first_reading, :second_reading

  # Reads the first paragraph of the given path as YAML metadata from the
  # Zettel, returning a hash of values
  def initialize(path)
    @hash = {}

    # Populate the hash
    File.read(path).split("\n\n", 2)[0].split("\n").each do |line|
      key, val = line.split(':').map { |x| x.strip }

      if val.include?(",")
        # If the value is a list (contains commas), make it an array, and either
        # add it do existing array or create a new one.
        vals = val.split("\s*,\s*")
        if @hash[key].is_a?(Array)
          @hash[key].concat(vals)
        else
          @hash[key] = vals
        end
      else
        # If the value is scalar, just ensure that we're adding to rather than
        # overwriting an existing hash entry.
        if @hash[key]
          if @hash[key].is_a?(Array)
            @hash[key].push(val)
          else
            @hash[key] = [ @hash[key], val ]
          end
        else
          @hash[key] = val
        end
      end
    end

    #
    # Populate the instance variables
    #

    # Required values
    if @hash['title'] && @hash['created']
      @title = @hash['title']
      @created = Time.parse(@hash['created'])
    else
      raise "The metadata is lacking 'title' and 'created' keys"
    end

    # Optional values
    @subtitle = @hash['subtitle']
    @oldname = @hash['oldname']
    @modified = Time.parse(@hash['modified']) unless @hash['modified'].nil?
    @first_reading = Time.parse(@hash['first_reading']) unless @hash['first_reading'].nil?
    @second_reading = Time.parse(@hash['second_reading']) unless @hash['second_reading'].nil?
  end
end

class Zettel
  @metadata
  attr_reader :type, :name, :kasten, :directory, :path

  def initialize(name)
    @name = name
  end

  # Returns true if the zettel exists where it should
  def exists?
    File.exists?(@path)
  end

  # Returns the Metadata for the Zettel
  def metadata
    if @metadata.is_a?(Metadata)
      @metadata
    else
      @metadata = Metadata.new(@path)
    end
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
    else
      raise TypeError, "Invalid name for a numerus currens Zettel: '#{name}'"
    end
  end

  # TODO: Get from zdatename
  def datename()
  end
end

class Tempus < Zettel
  @type = :iso8601
  attr_reader :time

  def initialize(name)
    super(name)

    if name =~ /^([a-z]+):(\d{8}T\d{4})$/
      @kasten = $1.to_sym
      @time = Time.parse(name)
      @directory = "#{Zettelkasten.dir(@kasten)}"
      @path = "#{@directory}/#{@name}#{Zettelkasten.ext}"
    else
      raise TypeError, "Invalid name for a tempus Zettel: '#{name}'"
    end
  end
end
