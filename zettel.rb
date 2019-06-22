# -----------------------------------------------------------------------------
#      Author: Richard Boyechko <rb-mercurial@diachronic.net>
# Description: A collection of methods for working with Zettel
#     Created: 2016-09-02
#    Comments:
# -----------------------------------------------------------------------------

require 'time'
require 'yaml'
require 'pathname'

#-------------------------------------------------------------------------------
# Zettelkasten
#-------------------------------------------------------------------------------

class Zettelkasten
  class << self; attr_accessor :root end
  class << self; attr_accessor :ext end

  # Default extension for Zettelkasten files
  @ext = ".txt"

  @root = Pathname(ENV['ZETTEL_DIR'] || File.expand_path('~/Dropbox/Zettel'))
  @kaesten = { "numerus"  => "main",  # main numerus kasten
               "tempus"   => "limbo", # main tempus kasten
               "tech"     => "tech",
               "personal" => "personal",
               "rp"       => "rp"
             }

  # Returns the directory for the given kasten
  def self.dir(kasten)
    if @kaesten[kasten]
      return @root + @kaesten[kasten]
    else
      raise "Unknown kasten '#{kasten}'"
    end
  end

  # Returns true if the given string names a valid kasten
  def self.kasten?(string)
    return true if @kaesten[string]
  end

  # Returns the Kasten that the Zettel path is under
  def self.kasten_of(path)
    p = Pathname(path)
    p = Pathname.pwd() + p unless p.absolute?
    relative = p.relative_path_from(@root)
    topmost_dir = relative.each_filename.to_a[0]
    return @kaesten.invert[topmost_dir]
  end

  # Returns the type of the zettel found at the given path
  def self.zettel_type(path)
    kasten_of(path) == "numerus" ? :numerus : :tempus
  end

  # Returns true if the given path is in the Zettelkasten
  def self.includes?(path)
    return true if kasten_of(path)
  end
end

#-------------------------------------------------------------------------------
# Zettel
#-------------------------------------------------------------------------------

class Zettel
  attr_reader :type,            # Zettel type; either :tempus or :numerus
              :kasten,          # Kasten, as string
              :slug,            # slug only (i.e. without Kasten)
              :link,            # full link (i.e. with Kasten, unless main)
              :path             # full path, as Pathname
  attr_accessor :metadata,      # hash of symbol -> value
                :text           # the text of the Zettel

  #
  # Custom Constructors
  #

  # Returns the appropriate Zettel subclass (Numerus or Tempus) of the Zettel
  # from the given link.
  def self.new_from_link(link)
    if Numerus.valid_link?(link)
      return Numerus.new_from_link(link)
    elsif Tempus.valid_link?(link)
      return Tempus.new_from_link(link)
    else
      return nil
    end
  end

  # Returns the appropriate Zettel subclass (Numerus or Tempus) of the Zettel
  # from the given path.
  def self.new_from_path(path)
    if Numerus.valid_path?(path)
      return Numerus.new_from_path(path)
    elsif Tempus.valid_path?(path)
      return Tempus.new_from_path(path)
    else
      return nil
    end
  end

  # More concise representation
  def inspect()
    return "#<#{self.class} @slug=#{@slug}, @kasten=#{@kasten}, "\
           "@path=#{@path.relative_path_from(Zettelkasten.root)}>"
  end

  # Returns true if the zettel exists where it should
  def exists?
    File.exists?(@path)
  end

  # Returns a relative path to Zettel from the Zettelkasten root
  def relative_path()
    if @path
      return @path.relative_path_from(Zettelkasten.root)
    else
      raise "The Zettel has no path set"
    end
  end

  # Reads the Zettel file, setting the @metadata and @text instance variables.
  def read_file()
    if File.readable?(@path)
      content = File.read(path).gsub(/\r\n/, "\n") # fix DOS line endings too
      metadata, @text = content.split("\n\n", 2)
      @metadata = from_quasi_yaml(metadata)
    else
      raise "The file for Zettel '#{@slug}' is not readable: #{@path}"
    end
  end

  # Writes to the Zettel file the content of metadata (in YAML) and @text.
  def write_file()
    if File.writable?(@path)
      File.write(@path, to_yaml(metadata) + "\n" + @text)
    else
      raise "The file for Zettel '#{@slug}' is not writable: #{@path}"
    end
  end

  # Replaces the links in the @text
  def replace_links(before, after)
    counter = 0
    @text = @text.gsub(/#{before}\]\]/) do |match|
      counter += 1
      "#{after}]]"
    end
    return counter
  end

  private

  # Returns a YAML block as a string, using inline sequence style.
  #
  # Can't use YAML::to_yaml() because it does not support inline style.
  def to_yaml(hash)
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

  METADATA_LINE = /^([[:alpha:]-]+): +(.+)$/
  ISO8601_PATTERN = /^\d{4}-\d{2}-\d{2}$/

  # Returns a hash representing the YAML block given.
  #
  # Can't use YAML::load() because it reads the value only up to a colon (I have
  # a lot of colons in Zettel titles) and converts numerus into a number. So
  # this is not really YAML anymore.
  def from_quasi_yaml(string)
    hash = Hash.new
    string.each_line.with_index do |line, index|
      if line =~ METADATA_LINE  # $1 = key, $2 = value
        # Fix legacy keys
        if $1 == "first-reading" || $1 == "second-reading"
          key = :readings
          value = [ $2 ]
        else
          key = $1.to_sym
          value = $2
        end

        if value =~ ISO8601_PATTERN
          begin
            value = Time.parse(value).to_date
          rescue ArgumentError
            $stderr.puts "Cannot parse #{value} as date in #{link}."
          end
        elsif value =~ /^\[(.*)\]$/      # YAML array
          value = $1.strip.split(/, */)
        end

        # Make sure multiple values for the same key are handled properly
        hash[key] = Array(hash[key]) if hash[key]
        if hash[key].is_a?(Array)
          hash[key] = hash[key].push(value)
        else
          hash[key] = value
        end
      else
        # Ignore otherwise
        STDERR.puts "Malformed line ##{index+1}?: #{line}"
      end
    end
    return hash
  end
end

class Numerus < Zettel
  SLUG_PATTERN = /^([0-9]{3})(-([a-z]+))*$/
  FQN_PATTERN = SLUG_PATTERN

  attr_reader :numerus,         # the number portion of the slug
              :litterae,        # the letter portion of thes lug
              :section          # the section of the numerus Kasten

  #
  # Custom Constructors
  #
  def self.new_from_path(*args)
    numerus = allocate
    numerus.init_path(*args)
    numerus
  end

  def self.new_from_link(*args)
    numerus = allocate
    numerus.init_link(*args)
    numerus
  end

  #
  # Custom Initializers
  #
  def init_path(path)
    if Zettelkasten.includes?(path)
      init_link(Pathname(path).basename(Zettelkasten.ext).to_s)
    else
      raise "The path is not part of the Zettelkasten: #{path}"
    end
  end

  def init_link(link)
    if link =~ FQN_PATTERN
      @type = :numerus
      @kasten = "numerus"
      @numerus = $1.to_i
      @litterae = $3
      reinit()
      read_file if @path.exist?
    else
      raise "This does not look like a Numerus Currens Zettel: #{link}"
    end
  end

  # Sets @slug, @section, and @path based on @numerus and @litterae
  def reinit()
    if @litterae.nil? or @litterae.empty?
      @litterae = ""
      @slug = "%03d" % @numerus
    else
      @slug = "%03d-#{@litterae}" % @numerus
    end
    @section = self.class.section_of(@slug)
    @path = Zettelkasten.dir(@kasten) + @section + (@slug + Zettelkasten.ext)
  end

  #
  # Instance Methods
  #

  # Returns the wiki link target
  def link()
    return @slug
  end

  def litterae=(litterae)
    if litterae =~ /[a-z]+/
      @litterae = litterae
      reinit
    else
      raise "Litterae can only be a string of letters, not '#{litterae}'"
    end
  end

  def numerus=(numerus)
    if numerus =~ /[0-9]{3}/
      @numerus = numerus
      reinit
    else
      raise "Numerus can only be three digits, not '#{numerus}'"
    end
  end

  #
  # Class Methods
  #

  # Returns the appropriate sub-directory in the numerus Kasten based on the
  # Zettel slug.
  def self.section_of(slug)
    if slug =~ SLUG_PATTERN
      num = $1.to_i
      if num >= 0 and num <= 99
        return "000-099"
      elsif num >= 100 and num <= 999
        return "#{slug[0]}00-#{slug[0]}99"
      else
        # Should never get here: SLUG_PATTERN limits the numerus to three digits
        raise "Numerus currens '#{slug}' is out of bounds (0-999)"
      end
    else
      raise "Slug '#{slug}' is not a numerus currens"
    end
  end

  # Returns true if the link (a string) is a valid link to Numerus Currens Zettel
  def self.valid_link?(string)
    return string =~ FQN_PATTERN ? true : false
  end

  # Returns true if the string is a valid path a to numerus currens zettel
  def self.valid_path?(string)
    if File.basename(string, Zettelkasten.ext) =~ SLUG_PATTERN &&
       Zettelkasten.kasten_of(string) == "numerus"
      return true
    else
      return false
    end
  end
end

#-------------------------------------------------------------------------------
# Tempus
#-------------------------------------------------------------------------------

class Tempus < Zettel
  FQN_PATTERN = /^(([a-z]+):)*(\d{8}T\d{4})$/
  SLUG_PATTERN = /^\d{8}T\d{4}$/

  attr_reader :time             # the time of the Zettel as a Time object

  #
  # Custom Constructors
  #
  def self.new_from_path(*args)
    numerus = allocate
    numerus.init_path(*args)
    numerus
  end

  def self.new_from_link(*args)
    numerus = allocate
    numerus.init_link(*args)
    numerus
  end

  #
  # Custom Initializers
  #
  def init_path(path)
    if Zettelkasten.includes?(path)
      init_link(Zettelkasten.kasten_of(path) +
                ":" +
                Pathname(path).basename(Zettelkasten.ext).to_s)
    else
      raise "The path is not part of the Zettelkasten: #{path}"
    end
  end

  def init_link(link)
    if link =~ FQN_PATTERN
      if $2.nil?
        @kasten = "tempus"
      else
        @kasten = $2
      end

      @slug = $3
      @type = :tempus
      @time = Time.parse(@slug)
      @path = Zettelkasten.dir(@kasten) + (@slug + Zettelkasten.ext)
      read_file if @path.exist?
    else
      raise "This does not look like a Tempus Zettel: #{link}"
    end
  end

  #
  # Instance Methods
  #

  # Returns the wiki link target
  def link()
    return "#{@kasten}:#{@slug}"
  end

  #
  # Class Methods
  #

  # Returns true if the link (a string) is a valid link to Tempus Zettel
  def self.valid_link?(string)
    return string =~ FQN_PATTERN ? true : false
  end

  # Returns true if this is a valid path a to numerus currens zettel
  def self.valid_path?(string)
    return File.basename(string, Zettelkasten.ext) =~ SLUG_PATTERN &&
           Zettelkasten.includes?(string) ? true : false
  end
end
