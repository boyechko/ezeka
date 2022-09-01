# coding: utf-8
# -----------------------------------------------------------------------------
#      Author: Richard Boyechko <hoybach@diachronic.net>
# Description: A collection of methods for working with Zettel
#     Created: 2016-09-02
#    Comments:
# -----------------------------------------------------------------------------

require 'time'
require 'yaml'
require 'pathname'
require 'open3'

#-------------------------------------------------------------------------------
# Zettelkasten
#-------------------------------------------------------------------------------

class Zettelkasten
  class << self; attr_accessor :root end
  class << self; attr_accessor :ext end
  class << self; attr_reader :default_kasten end

  # Default extension for Zettelkasten files
  @ext = ".txt"

  # Abort if $ZETTEL_DIR is not set
  if File.exists?(Pathname(ENV['ZETTEL_DIR']))
    @root = Pathname(ENV['ZETTEL_DIR'])
  else
    raise "$ZETTEL_DIR is not set"
  end

  # Kasten Name => ID type
  @kaesten = { "os"        => :tempus,          # things get "ingested"
               "esophagus" => :bolus,           # regurgitated ingesta (needs processing) FIXME
               "rumen"     => :numerus,         # [default] others' ideas
               # "reticulum" is not used, so "rumen" stands for "reticulorumen"
               "omasum"    => :tempus,          # [default] permanent notes
               "abomasum"  => :tempus,          # snippets and drafts
               "rectum"    => :tempus,          # submitted manuscripts
               "machina"   => :tempus,          # technical notes
               "fabula"    => :tempus           # roleplaying notes
             }

  # The default kaesten can be referred to without specifying their kasten
  # ID type => Kasten
  @default_kasten = { :numerus => "rumen",
                      :tempus => "omasum",
                      :bolus => "esophagus" # FIXME: temporary
                    }

  # Translations for backward compatibility
  # Old Kasten => Current Kasten
  @kaesten_old = { }

  # Returns the directory for the given kasten
  def self.dir(kasten)
    if @kaesten[kasten]
      return @root + kasten
    elsif @kaesten_old[kasten]
      return @root + @kaesten_old[kasten]
    else
      raise "Unknown kasten '#{kasten}'"
    end
  end

  # Returns true if the given string names a valid kasten
  def self.kasten?(string)
    return true if @kaesten[string] or @kaesten_old[string]
  end

  # Returns the Kasten that the Zettel path is under
  def self.kasten_of(path)
    p = Pathname(path)
    p = @root + p unless p.absolute?
    relative = p.relative_path_from(@root)
    topmost_dir = relative.each_filename.to_a[0]
    return topmost_dir if @kaesten[topmost_dir]
  end

  # Returns the ID type of the zettel found at the given path
  def self.id_type_of(path)
    return @kaesten[kasten_of(path)]
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
  attr_reader :id_type,         # Zettel ID type: :tempus, :numerus, or :bolus (FIXME)
              :kasten,          # Kasten, as string
              :id,              # id only (i.e. without Kasten)
              :link,            # full link (i.e. with Kasten, unless default)
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
    elsif Bolus.valid_link?(link) # FIXME: temporary
      return Bolus.new_from_link(link)
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
    elsif Bolus.valid_path?(path) # FIXME: temporary
      return Bolus.new_from_path(path)
    else
      return nil
    end
  end

  # More concise representation
  def inspect()
    return "#<#{self.class} @id=#{@id}, @kasten=#{@kasten}, "\
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
      @text = "" if @text.nil?
      return true
    else
      raise "The file for Zettel '#{@id}' is not readable: #{@path}"
    end
  end

  # Writes to the Zettel file the content of metadata (in YAML) and @text.
  def write_file()
    if File.writable?(@path)
      File.write(@path, yaml_metadata() + "\n" + @text)
    else
      raise "The file for Zettel '#{@id}' is not writable: #{@path}"
    end
  end

  # Returns a list of Zettel that have links to the current Zettel
  def links_to()
    links = Array.new()

    Dir.chdir(Zettelkasten.root)
    cmd = "grep --include=*#{Zettelkasten.ext} -lR -e '#{id}\\]\\]' -e 'parent: #{id}' *"
    Open3.popen3(cmd) do |stdin, stdout, stderr|
      while file = stdout.gets
        z = Zettel.new_from_path(Zettelkasten.root + file.chomp)

        if z
          links.push(z)
        else
          raise "Cannot create Zettel from file #{file}"
        end
      end
    end

    return links
  end

  # Replaces the links in the @text and @metadata[:parent].
  def replace_links(before, after)
    counter = 0
    if @metadata[:parent] &&
       @metadata[:parent].gsub!(/#{before}/, "#{after}")
      counter += 1
    end

    @text = @text.gsub(/#{before}\]\]/) do |match|
      counter += 1
      "#{after}]]"
    end
    return counter
  end

  # Removes the links to BEFORE in the @text, returning number of links removed
  def remove_links(before)
    counter = 0
    @text = @text.gsub(/#{before}\]\]/) do |match|
      counter += 1
      "REMOVED_#{before}]]"
    end
    return counter
  end

  private

  METADATA_KEYS = [:rubric, :subtitle, :author,
                   :created, :modified,
                   :parent, :firstborn, :oldnames,
                   :keywords, :readings]

  METADATA_LINE = /^([[:alpha:]-]+): +(.+)$/
  COMMENT_LINE = /^#/
  TIME_PATTERN = /^\d{4}-\d{2}-\d{2}( [[:alpha:]]{3} \d{2}:\d{2})*$/
  METADATA_TIME_FORMAT = "%F %a %H:%M" # Org-mode timestamp format

  # String that comes just before the id
  ID_PREFIX = "§"
  # String used to separate the id from the title
  ID_TITLE_SEPARATOR = " "
  # Regexp matching separation between the end of id and beginning of title
  ID_REGEXP = /[^ ]+/

  # Returns a YAML block as a string, using inline sequence style.
  #
  # Can't use YAML::to_yaml() because it does not support inline style.
  def yaml_metadata()
    result = ""

    # Make sure the rubric has the correct id
    @metadata[:rubric].gsub!(/#{ID_PREFIX}#{ID_REGEXP}/,
                            "#{ID_PREFIX}#{@link}")

    # Output the metadata in the order specified in METADATA_KEYS
    METADATA_KEYS.each { |key|
      result += "#{to_yaml_line(key)}\n" if @metadata[key]
    }

    return result
  end

  # Returns a formatted YAML line for the given metadata key, treating the value
  # types appropriately.
  def to_yaml_line(key)
    val = @metadata[key]
    if val.is_a?(Array)
      return "#{key}: [ #{val.join(', ')} ]"
    elsif val.is_a?(Time)
      return "#{key}: #{val.strftime(METADATA_TIME_FORMAT)}"
    else
      return "#{key}: #{val}"
    end
  end

  # Returns a hash representing the YAML block given.
  #
  # Can't use YAML::load() because it reads the value only up to a colon (I have
  # a lot of colons in Zettel titles) and converts numerus into a number. So
  # this is not really YAML anymore.
  def from_quasi_yaml(string)
    hash = Hash.new
    string.each_line.with_index do |line, index|
      if line =~ METADATA_LINE  # $1 = key, $2 = value
        key = $1.to_sym
        value = $2

        if value =~ TIME_PATTERN
          begin
            value = Time.parse(value)
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
      elsif line =~ COMMENT_LINE
        # Skip comment lines
      else
        # Ignore otherwise
        STDERR.puts "Malformed line ##{index+1}?: #{line}"
      end
    end
    return hash
  end
end

#-------------------------------------------------------------------------------
# Numerus
#-------------------------------------------------------------------------------

class Numerus < Zettel
  ZETTEL_ID_TYPE = :numerus

  N_DIGITS = 4                  # number of digits
  SEPARATOR = "-"               # separator between digits and letters
  N_LETTERS = 1                 # number of letters
  ID_PATTERN = /^(?<letters>[a-z]{#{N_LETTERS}})#{SEPARATOR}(?<digits>[0-9]{#{N_DIGITS}})$/
  FQN_PATTERN = ID_PATTERN

  attr_reader :digits,          # the number portion of the id
              :letters,         # the letter portion of the id
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

  # For self.class::Constant, see https://stackoverflow.com/a/13234497
  def init_link(link)
    if link =~ self.class::FQN_PATTERN
      @id_type = self.class::ZETTEL_ID_TYPE
      @kasten = Zettelkasten.default_kasten[self.class::ZETTEL_ID_TYPE]
      @link = link
      @digits = Regexp.last_match.named_captures["digits"]
      @letters = Regexp.last_match.named_captures["letters"]
      reinit()
      read_file if @path.exist?
    else
      raise "This does not look like a(n) #{ZETTEL_ID_TYPE.to_s} currens Zettel: #{link}"
    end
  end

  # Sets @id, @section, and @path based on @digits and @letters
  def reinit()
    @id = self.id()
    @section = self.class.section_of(@id)
    @path = Zettelkasten.dir(@kasten) + @section + (@id + Zettelkasten.ext)
  end

  #
  # Instance Methods
  #

  # Returns the wiki link target
  def link()
    return @id
  end

  def id()
    return @letters + self.class::SEPARATOR + @digits
  end

  def digits=(digits)
    if digits =~ /[0-9]{#{self.class::N_DIGITS}}/
      @digits = digits
      reinit
    else
      raise "Digits can only be #{self.class::N_DIGITS} digit(s), not '#{digits}'"
    end
  end

  def letters=(letters)
    if letters =~ /[a-z]{#{self.class::N_LETTERS}}/
      @letters = letters
      reinit
    else
      raise "Letters can only be #{self.class::N_LETTERS} English letter(s), not '#{letters}'"
    end
  end

  #
  # Class Methods
  #

  # Returns the appropriate sub-directory in the numerus Kasten based on the
  # Zettel ID.
  def self.section_of(id)
    if id =~ ID_PATTERN
      return "#{id[0]}"
    else
      raise "ID '#{id}' is not a numerus currens #{ID_PATTERN}"
    end
  end

  # Returns true if the link (a string) is a valid link to numerus currens Zettel
  def self.valid_link?(string)
    return string =~ self::FQN_PATTERN ? true : false
  end

  # Returns true if the string is a valid path a to numerus currens zettel
  def self.valid_path?(string)
    if File.basename(string, Zettelkasten.ext) =~ self::ID_PATTERN &&
       Zettelkasten.kasten_of(string) == Zettelkasten.default_kasten[self::ZETTEL_ID_TYPE]
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
  ID_PATTERN = /^\d{8}T\d{4}$/

  attr_reader :time             # the time of the Zettel as a Time object

  #
  # Custom Constructors
  #
  def self.new_from_path(*args)
    tempus = allocate
    tempus.init_path(*args)
    tempus
  end

  def self.new_from_link(*args)
    tempus = allocate
    tempus.init_link(*args)
    tempus
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
        @kasten = Zettelkasten.default_kasten[:tempus]
      else
        @kasten = $2
      end

      @id_type = :tempus
      @id = $3
      @link = self.link
      @time = Time.parse(@id)
      @path = Zettelkasten.dir(@kasten) + @time.year.to_s + (@id + Zettelkasten.ext)
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
    if @kasten == Zettelkasten.default_kasten[@id_type] then return @id
    else return "#{@kasten}:#{@id}"
    end
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
    return File.basename(string, Zettelkasten.ext) =~ ID_PATTERN &&
           Zettelkasten.includes?(string) ? true : false
  end
end

#-------------------------------------------------------------------------------
# Bolus (Temporary)
#-------------------------------------------------------------------------------

class Bolus < Numerus
  ZETTEL_ID_TYPE = :bolus

  N_LETTERS = 3
  SEPARATOR = "-"
  N_DIGITS = 3
  ID_PATTERN = /^(?<digits>[0-9]{#{N_DIGITS}})#{SEPARATOR}(?<letters>[a-z]{#{N_LETTERS}})$/
  FQN_PATTERN = ID_PATTERN

  # How to form ids
  def id()
    return @digits + self.class::SEPARATOR + @letters
  end

  # Returns the appropriate sub-directory in the bolus Kasten based on the
  # Zettel id.
  def self.section_of(id)
    if id =~ self::ID_PATTERN
      num = $1.to_i
      if num >= 0 and num <= 99
        return "000-099"
      elsif num >= 100 and num <= 999
        return "#{id[0]}00-#{id[0]}99"
      else
        # Should never get here: ID_PATTERN limits the numerus to three digits
        raise "Numerus currens '#{id}' is out of bounds (0-999)"
      end
    else
      raise "ID '#{id}' is not a bolus currens"
    end
  end
end
