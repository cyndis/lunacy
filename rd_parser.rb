require_relative 'str_scanner'
if not defined? Lunacy
  require_relative 'lunacy'
end

class Lunacy::RdParser
  def initialize(str)
    @scanner = Scanner.new(str)
  end
  attr_reader :scanner

  def e!(msg, die=true, s=nil)
    s ||= scanner
    line = s.string.split("\n")[s.line]
    $stderr.puts "#{s.line+1}:#{s.column+1}: #{msg}"
    $stderr.puts line
    $stderr.puts(' ' * s.column + '`-- here')
    parse_chain = caller.map do |str|
      str =~ /`(.+?)'/
      $1
    end.select { |m| m.start_with? 'parse_' }.map do |m|
      m[6..-1]
    end
    $stderr.puts 'in ' + parse_chain.join(', ')
    exit if die
  end

  def parse_space(s)
    if m = s.scan_str(/\s+/)
      true
    end
  end

  def parse_space?(s)
    if m = s.scan_str(/\s*/)
      true
    end
    true
  end

  def parse_stmt_end(s)
    if s.scan_str(/\s*?\n|;/)
      parse_space? s
      true
    else
      false
    end
  end

  def parse_integer(s)
    if m = s.scan_str(/[1-9][0-9]*|0/)
      m.to_i
    end
  end

  def parse_string_escape(s)
    if s.scan_str(/\\n/)
      "\n"
    end
  end

  def parse_string(s)
    if s.scan_str(/"/)
      symbols = []
      while not s.scan_str(/"/)
        symbols << (parse_string_escape(s) || s.scan_str(/./))
      end

      symbols.join
    end
  end

  def parse_word(s)
    if not s.lookahead(/(def)|(end)|(return)|(extern)/) and
        m = s.scan_str(/[a-zA-Z_][a-zA-Z0-9_]*/)
      m
    end
  end

  def parse_typename(s)
    if m = parse_word(s)
      ptr_stars = s.scan_str(/\**/)
      [:Type, m, ptr_stars.length]
    end
  end

  def parse_def_arg(s)
    if type = parse_typename(s)
      t = s.push
      name = nil
      if parse_space(t) and name = parse_word(t)
        t.commit
      end
      [:TypedName, name, type]
    end
  end

  def parse_def_args(s)
    if s.scan_str(/\(/) and parse_space?(s)
      args = []
      while not s.scan_str(/\)/)
        parse_space? s
        arg = parse_def_arg(s)
        if not arg
          e! 'Expected argument or closing parenthesis'
        end
        args << arg

        parse_space? s
        s.scan_str /,/
      end
      args
    end
  end

  def parse_root(s)
    parse_space? s

    stmts = []
    while stmt = parse_root_stmt(s)
      stmts << stmt
      parse_stmt_end(s)
    end

    if not s.eos?
      e! 'Expected top-level statement'
    end

    [:TranslationUnit, stmts]
  end

  def parse_root_stmt(s)
    parse_extern_stmt(s) || parse_def_stmt(s) || parse_type_stmt(s)
  end

  def parse_fun_decl(s)
    return false if not s.scan_str(/def/)
    e! "Expected whitespace" unless parse_space(s)
    e! "Expected function name" unless name = parse_word(s)
    e! "Expected function argument list" unless args = parse_def_args(s)
    parse_space? s
    e! "Expected ->" unless s.scan_str(/->/)
    parse_space? s
    e! "Expected typename" unless type = parse_typename(s)

    [name, type, args]
  end

  def parse_extern_stmt(s)
    t = s.push
    if t.scan_str(/extern/)
      t.commit

      e! 'Expected whitespace' unless parse_space(s)

      decl = parse_fun_decl(s)
      e! 'Expected function declaration' unless decl

      [:ExternFunction, *decl]
    end
  end

  def parse_def_stmt(s)
    if decl = parse_fun_decl(s)
      e! "Expected newline or semicolon" unless parse_stmt_end(s)

      stmts = []
      while not s.scan_str(/end/)
        parse_space? s
        stmt = parse_fun_stmt(s)
        if not stmt
          e! 'Expected statement'
        end
        stmts << stmt

        parse_stmt_end(s)
      end

      [:Function, *decl, stmts]
    end
  end

  def parse_type_stmt(s)
    if s.scan_str(/type/)
      e! "Expected whitespace" unless parse_space(s)
      e! "Expected name" unless name = parse_word(s)
      parse_space? s
      e! "Expected =" unless s.scan_str(/=/)
      parse_space? s
      e! "Expected type name" unless type = parse_typename(s)

      [:Typedef, name, type]
    end
  end

  def parse_fun_stmt(s)
    parse_expr(s) || parse_return(s)
  end

  def parse_return(s)
    if s.scan_str(/return/)
      t = s.push
      ret_val = nil
      if parse_space(t) and ret_val = parse_expr(t)
        t.commit
      end

      [:Return, s.xy, ret_val]
    end
  end

  def parse_expr(s)
    parse_binary_expr(s) || parse_value_expr(s)
  end

  def parse_expr_list(s)
    exprs = []

    parse_space? s

    while expr = parse_expr(s)
      exprs << expr
      parse_space? s
      break unless s.scan_str(/,/)
      parse_space? s
    end

    exprs
  end

  def parse_value_expr(s)
    parse_funcall(s) || parse_paren_expr(s) || parse_int_literal(s) ||
        parse_str_literal(s)
  end

  def parse_int_literal(s)
    if m = parse_integer(s)
      [:IntLiteral, s.xy, m]
    end
  end

  def parse_str_literal(s)
    if m = parse_string(s)
      [:StrLiteral, s.xy, m]
    end
  end

  def parse_binary_expr(s)
    t = s.push
    if left = parse_value_expr(t)
      parse_space? t
      bin_op = t.scan_str(/\+/)
      return false unless bin_op
      t.commit

      parse_space? s
      right = parse_expr(s)
      e! "Expected expression" unless right

      [:Add, s.xy, left, right]
    end
  end

  def parse_funcall(s)
    parse_standard_funcall(s) || parse_chained_funcall(s) ||
        parse_varref(s)
  end

  def parse_standard_funcall(s)
    t = s.push
    if name = parse_word(t) and t.scan_str(/\(/) and pos = t.xy
      t.commit

      args = parse_expr_list(s)

      e! 'Expected closing parenthesis' unless s.scan_str(/\)/)

      return [:Funcall, pos, name, args]
    end
    t = s.push
    if name = parse_word(t) and parse_space(t) and pos = t.xy
      t.commit

      args = parse_expr_list(s)

      return [:Funcall, pos, name, args]
    end
  end

  def parse_chained_funcall(s)
    t = s.push
    if receiver = (parse_paren_expr(t)||parse_int_literal(t)||
                   parse_str_literal(t)||parse_standard_funcall(t)||
                   parse_varref(t)) and
                   t.scan_str(/\./)
      t.commit

      call = parse_standard_funcall(s) || parse_varref(s)
      e! 'Expected function name' unless call
      cur_chain = [:Funcall, s.xy, call[2], [receiver] + call[3]]
      while s.scan_str(/\./)
        call = parse_standard_funcall(s) || parse_varref(s)
        e! 'Expected function name' unless call
        cur_chain = [:Funcall, s.xy, call[2], [cur_chain] + call[3]]
      end

      cur_chain
    end
  end

  def parse_varref(s)
    if m = parse_word(s)
      [:Funcall, s.xy, m, []]
    end
  end

  def parse_paren_expr(s)
    if m = s.scan_str(/\(/)
      expr = parse_expr(s)
      e! 'Expected expression' unless expr
      e! 'Expected closing parenthesis' unless s.scan_str(/\)/)
      expr
    end
  end

  def parse
    parse_root scanner
  end
end

if __FILE__ == $0
  parser = Lunacy::RdParser.new(ARGV[0])
  require 'pp'
  pp parser.send('parse_'+ARGV[1], parser.scanner)
end
