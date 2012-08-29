if (not defined? RUBY_ENGINE or not RUBY_ENGINE == 'rbx')
  class Regexp
    def match_start(str, idx)
      Regexp.compile('\A(?:'+source+')').match(str, idx)
    end
  end
end

class Scanner
  def initialize(str, parent=nil)
    @string = str
    @pos = 0
    @line = 0
    @column = 0
    @parent = parent
  end
  attr_accessor :string, :pos, :line, :column, :prev_line, :prev_column

  def rest
    string[pos..-1]
  end

  def xy
    ret = [string, line, column]
    def ret.inspect
      '<pos>'
    end
    ret
  end

  def push
    child = Scanner.new(string, self)
    child.string, child.pos, child.line, child.column, child.prev_line,
        child.prev_column = string, pos, line, column, prev_line, prev_column
    child
  end

  def commit
    @parent.pos, @parent.line, @parent.column, @parent.prev_line,
        @parent.prev_column = pos, line, column, prev_line, prev_column
  end

  def advance_str(str)
    self.prev_line = line
    self.prev_column = column
    self.pos += str.length
    self.line += str.count("\n")
    if (str.include?("\n"))
      self.column = str.length - str.rindex("\n")
    else
      self.column += str.length
    end
  end

  def scan(regexp)
    if (match = regexp.match_start(rest, 0))
      advance_str match.to_s
      match.captures
    else
      nil
    end
  end

  def scan_str(regexp)
    if (match = regexp.match_start(rest, 0))
      advance_str match.to_s
      match.to_s
    else
      nil
    end
  end

  def lookahead(regexp)
    if (match = regexp.match_start(rest, 0))
      true
    else
      false
    end
  end

  def eos?
    pos == string.length
  end
end
