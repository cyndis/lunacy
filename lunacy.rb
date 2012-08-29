module Lunacy
  def self.temporary
    @next_temporary ||= 'a'
    @next_temporary.succ!
    return "%#@next_temporary"
  end

  def self.cast(io, expr, type)
    if expr.type == type
      return expr.generate_ir(io)
    end

    to = type
    from = expr.type

    if from.int? and to.int?
      temp = Lunacy.temporary

      if to.bit_size < from.bit_size
        expr.w! "Truncating expression from #{from.bit_size} to #{to.bit_size}"\
            " bits"
        inst = 'trunc'
      elsif to.bit_size > from.bit_size
        inst = 'zext'
      else
        inst = 'bitcast'
      end

      io.puts "#{temp} = #{inst} #{from.llvm_type} #{expr.generate_ir(io)} to "\
                "#{to.llvm_type}"
      return temp
    elsif to.is_a?(Lunacy::DiscriminatingType) and to.subtype.like? from
      # Literals can be implicitly casted from the base type to a discriminated
      # type
      if expr.is_a?(Lunacy::FirstClassExpression)
        temp = Lunacy.temporary
        io.puts "#{temp} = bitcast #{from.llvm_type} #{expr.generate_ir(io)} "\
            "to #{to.llvm_type}"
        return temp
      else
        expr.e! "Cannot implicitly cast type into its discriminating type"
      end
    elsif from.pointer? and to.pointer?
      expr.e! "Invalid implicit cast from pointer type `#{from.human_type}' to "\
          "`#{to.human_type}'"
      temp = Lunacy.temporary
      io.puts "#{temp} = bitcast #{from.llvm_type} #{expr.generate_ir(io)} to "\
               "#{to.llvm_type}"
      return temp
    else
      raise 'Invalid typecast'
    end
  end

  def self.execute(callable)
    require 'stringio'

    io = StringIO.new
    callable.generate_ir io

    $stderr.puts '-- running --'
    IO.popen('lli - 2>&1', 'r+') do |lli|
      lli << io.string
      lli.close_write

      print lli.read
    end
    $stderr.puts '-- ended --'
  end
end

module Lunacy
  class Type
    def initialize(is_int, bit_size, signed, llvm_type)
      @is_int, @bit_size, @signed = is_int, bit_size, signed
      @llvm_type = llvm_type
    end

    def int?
      @is_int
    end

    def signed?
      @signed
    end

    def bit_size
      @bit_size
    end

    def llvm_type
      @llvm_type
    end

    def pointer?
      false
    end

    def human_type
      if int?
        "i#{bit_size}"
      else
        "unknown"
      end
    end

    def like?(other)
      other == self || other.like?(self)
    end
  end

  class PointerType
    def initialize(pointee_type)
      @pointee = pointee_type
    end
    attr_reader :pointee

    def int?
      false
    end

    def signed?
      false
    end

    def bit_size
      64 # FIXME lie
    end

    def llvm_type
      "#{pointee.llvm_type}*"
    end

    def pointer?
      true
    end

    def human_type
      "#{pointee.human_type}*"
    end

    def like?(other)
      other == self || (
        other.is_a?(PointerType) and other.pointee.is_a?(VectorType) and
        other.pointee.element.like?(pointee)
      )
    end
  end

  class VectorType
    def initialize(length, element_type)
      @length = length
      @element = element_type
    end
    attr_reader :length, :element

    def int?
      false
    end

    def signed?
      false
    end

    def bit_size
      element.bit_size * length
    end

    def llvm_type
      "[#{length} x #{element.llvm_type}]"
    end

    def pointer?
      false
    end

    def human_type
      "#{length}x #{element.human_type}"
    end

    def like?(other)
      other == self || other.like?(self)
    end
  end

  class DiscriminatingType
    def initialize(name, subtype)
      @name, @subtype = name, subtype
    end
    attr_reader :subtype, :name

    def int?
      subtype.int?
    end

    def signed?
      subtype.signed?
    end

    def bit_size
      subtype.bit_size
    end

    def llvm_type
      subtype.llvm_type
    end

    def pointer?
      subtype.pointer?
    end

    def human_type
      "#{name}(a.k.a. #{subtype.human_type})"
    end

    def like?(other)
      other == self || other.like?(self)
    end
  end

  I32 = Type.new(true, 32, true, :i32)
  I16 = Type.new(true, 16, true, :i16)
  I8 = Type.new(true, 8, true, :i8)
  Void = Type.new(false, 0, false, :void)

  I8Ptr = PointerType.new(I8)
  I8PtrPtr = PointerType.new(I8Ptr)
end

class Lunacy::Scope
  def initialize(parent)
    @parent = parent
    @symbols = {}
  end
  attr_reader :symbols, :parent

  def find_symbol(name)
    if s = symbols[name]
      return s
    elsif parent
      return parent.find_symbol(name)
    else
      nil
    end
  end
end

class Lunacy::TranslationUnit < Lunacy::Scope
  def initialize
    super(nil)

    @next_global = '.a'

    @data_globals = []
    @externs = []
    @functions = []
    @types = {'i8' => Lunacy::I8,
              'i16' => Lunacy::I16,
              'i32' => Lunacy::I32,
              'void' => Lunacy::Void}
  end
  attr_reader :data_globals, :externs, :functions, :types

  def add_global_data(data)
    name = '@'+@next_global
    @next_global.succ!

    @data_globals << [name, data]

    return name, Lunacy::PointerType.new(
      Lunacy::VectorType.new(data.length, Lunacy::I8))
  end

  def add_extern_function(name, return_type, parameters)
    sym = Lunacy::FunctionSymbol.new(name, return_type, parameters)
    @externs << sym
    symbols[name] = sym
  end

  def add_function(function)
    @functions << function
    symbols[function.name] = function
  end

  def add_typedef(name, type)
    @types[name] = Lunacy::DiscriminatingType.new(name, type)
  end

  def generate_ir(io)
    @data_globals.each do |(name, data)|
      data_str = "[#{data.length} x i8] c\""\
                 "#{data.map { |ch| '\\%02x'%ch }.join}\""
      io.puts "#{name} = private unnamed_addr constant #{data_str}, align 1"
    end

    @externs.each do |sym|
      io.puts "declare #{sym.return_type.llvm_type} @#{sym.name}"\
          "(#{sym.parameters.map(&:type).map(&:llvm_type).join(', ')})"
    end

    @functions.each do |fun|
      fun.generate_ir io
    end
  end
end

class Lunacy::Function < Lunacy::Scope
  def initialize(name, return_type, parent)
    super(parent)

    @name = name
    @return_type = return_type
    @statements = []
    @parameters = []
  end
  attr_reader :name, :statements, :parameters, :return_type

  def generate_ir(io)
    statements.each do |stmt|
      stmt.function = self
      stmt.scope = self
    end

    parm_str = parameters.map do |parm|
      "#{parm.type.llvm_type} #{parm.llvm_name}"
    end.join(", ")

    io.puts "define i32 @#{name}(#{parm_str}) nounwind uwtable {"
    statements.each do |stmt|
      stmt.generate_ir io
    end
    io.puts "}"
  end
end

class Lunacy::FunctionSymbol
  def initialize(name, return_type, parameters)
    @name, @return_type, @parameters = name, return_type, parameters
  end
  attr_reader :name, :return_type, :parameters
end

class Lunacy::VariableSymbol
  def initialize(name, type, llvm_name)
    @name, @type, @llvm_name = name, type, llvm_name
  end
  attr_reader :name, :type, :llvm_name
end

class Lunacy::TypedName
  def initialize(name, type)
    @name, @type = name, type
  end
  attr_reader :name, :type
end

class Lunacy::ScopedObject
  attr_accessor :function, :scope
  attr_reader :pos

  def e!(msg)
    $stderr.puts "#{pos[1]+1}:#{pos[2]+1}: #{msg}"
    $stderr.puts pos[0].split("\n")[pos[1]]
    $stderr.puts(' ' * pos[2] + '`-- here')
    exit
  end

  def w!(msg)
    $stderr.puts "#{pos[1]+1}:#{pos[2]+1}: Warning: #{msg}"
    $stderr.puts pos[0].split("\n")[pos[1]]
    $stderr.puts(' ' * pos[2] + '`-- here')
  end
end

class Lunacy::ReturnStatement < Lunacy::ScopedObject
  def initialize(pos, parameter)
    @pos = pos
    @parameter = parameter
  end
  attr_reader :parameter

  def scope=(s)
    super
    parameter.scope = s
  end

  def generate_ir(io)
    value = Lunacy.cast(io, parameter, Lunacy::I32)
    io.puts "ret i32 #{value}"
  end
end

class Lunacy::FirstClassExpression < Lunacy::ScopedObject
  def initialize(pos, value, type)
    @pos = pos
    @value = value
    @type = type
  end
  attr_reader :value, :type

  def generate_ir(io)
    return "#{value}"
  end
end

class Lunacy::AddExpression < Lunacy::ScopedObject
  def initialize(pos, left, right)
    @pos = pos
    @left = left
    @right = right
  end
  attr_reader :left, :right

  def scope=(s)
    super
    left.scope = right.scope = s
  end

  def type
    @left.type
  end

  def generate_ir(io)
    temp = Lunacy.temporary
    io.puts "#{temp} = add #{left.type.llvm_type} #{left.generate_ir(io)}, "\
            "#{Lunacy.cast(io, right, left.type)}"
    return temp
  end
end

class Lunacy::FuncallExpression < Lunacy::ScopedObject
  def initialize(pos, name, parameters)
    @pos = pos
    @name, @parameters = name, parameters
  end
  attr_reader :name, :parameters

  def scope=(s)
    super
    parameters.each { |parm| parm.scope = s }
  end

  def target
    tgt = scope.find_symbol name
    if not tgt
      raise 'Symbol not found'
    end
    tgt
  end

  def type
    case target
    when Lunacy::FunctionSymbol, Lunacy::Function
      target.return_type
    when Lunacy::VariableSymbol
      target.type
    end
  end

  def generate_ir(io)
    case target
    when Lunacy::FunctionSymbol, Lunacy::Function
      temp = Lunacy.temporary
      if parameters.length != target.parameters.length
        e! "Expected #{target.parameters.length} parameters, "\
            "#{parameters.length} given"
      end
      parms = parameters.zip(target.parameters).map do |(parm, expected_parm)|
        expected_type = expected_parm.type
        "#{expected_type.llvm_type} #{Lunacy.cast io, parm, expected_type}"
      end.join(', ')
      if type != Lunacy::Void
        temp_str = "#{temp} = "
      else
        temp_str = ""
      end
      io.puts "#{temp_str}call ccc #{type.llvm_type} @#{target.name}(#{parms})"
      if type != Lunacy::Void
        return temp
      else
        return nil
      end
    when Lunacy::VariableSymbol
      target.llvm_name
    end
  end
end

module Lunacy::AST
  class TranslationUnit < Struct.new(:nodes)
    def generate
      tu = Lunacy::TranslationUnit.new
      nodes.each do |node|
        if (node.generate(tu))
          raise 'Non-statement used in tu context'
        end
      end
      tu
    end
  end

  class Function < Struct.new(:name, :return_type, :parameters, :body)
    def generate(tu)
      fun = Lunacy::Function.new(name, return_type.generate(tu), tu)
      body.each do |stmt|
        ir_stmt = stmt.generate(tu)
        fun.statements << ir_stmt if ir_stmt
      end
      parameters.each do |parm|
        parm = parm.generate(tu)
        var = Lunacy::VariableSymbol.new(parm.name, parm.type, Lunacy.temporary)
        fun.parameters << var
        fun.symbols[parm.name] = var
      end
      tu.add_function fun
      nil
    end
  end

  class ExternFunction < Struct.new(:name, :return_type, :parameters)
    def generate(tu)
      tu.add_extern_function name, return_type.generate(tu),
          parameters.map { |t| t.generate(tu) }
      nil
    end
  end

  class Typedef < Struct.new(:name, :type)
    def generate(tu)
      tu.add_typedef name, type.generate(tu)
      nil
    end
  end

  class Return < Struct.new(:pos, :value)
    def generate(tu)
      Lunacy::ReturnStatement.new(pos,
        value ? value.generate(tu) :
                Lunacy::FirstClassExpression.new(0, Lunacy::I32))
    end
  end

  class IntLiteral < Struct.new(:pos, :value)
    def generate(tu)
      if value.abs > 0xFFFF
        type = Lunacy::I32
      elsif value.abs > 0xFF
        type = Lunacy::I16
      else
        type = Lunacy::I8
      end
      Lunacy::FirstClassExpression.new(pos, value, type)
    end
  end

  class Add < Struct.new(:pos, :left, :right)
    def generate(tu)
      Lunacy::AddExpression.new(pos, left.generate(tu), right.generate(tu))
    end
  end

  class Funcall < Struct.new(:pos, :name, :parameters)
    def generate(tu)
      Lunacy::FuncallExpression.new(pos, name, parameters.map{|pm| pm.generate(tu)})
    end
  end

  class StrLiteral < Struct.new(:pos, :value)
    def generate(tu)
      name, type = tu.add_global_data(value.encode('UTF-8').bytes.to_a + [0])
      Lunacy::FirstClassExpression.new(pos, name, type)
    end
  end

  class Variable < Struct.new(:pos, :name)
    def generate(tu)
      Lunacy::VariableExpression.new(pos, name)
    end
  end

  class Type < Struct.new(:name, :pointer_level)
    def generate(tu)
      type = tu.types[name]
      pointer_level.times do
        type = Lunacy::PointerType.new(type)
      end
      type
    end
  end

  class TypedName < Struct.new(:name, :type)
    def generate(tu)
      Lunacy::TypedName.new(name, type.generate(tu))
    end
  end
end

def Lunacy.build_ast(array)
  require 'pp'
  obj = Lunacy::AST.const_get(array[0])
  args = array[1..-1].map do |arg|
    if arg.is_a?(Array)
      if arg[0].is_a?(Symbol)
        Lunacy.build_ast(arg)
      elsif arg[0].is_a?(Array) and arg[0][0].is_a?(Symbol)
        arg.map { |subarg| Lunacy.build_ast(subarg) }
      else
        arg
      end
    else
      arg
    end
  end
  obj.new(*args)
end

class Lunacy::Interface
  def run_file(path)
    io = path == '-' ? $stdin : File.open(path, 'r')

    require_relative 'rd_parser'

    parser = Lunacy::RdParser.new(io.read)
    ast = parser.parse
    require 'pp'
    tu = Lunacy.build_ast(ast).generate

    Lunacy.execute tu
  end
end

if __FILE__ == $0
  intf = Lunacy::Interface.new
  intf.run_file ARGV[0] || '-'
end
