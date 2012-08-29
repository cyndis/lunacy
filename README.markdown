# Lunacy

Lunacy is an LLVM bitcode-targeting compiler for a C-style language with some
Ruby influences. Both the language and the compiler are incomplete and serve
more as a tech demo and base for language and compiler development
experimentation. For example, there is currently no support for any kind of
conditional expressions or even local variables (apart from function arguments).
It does, however, support both intra-file function calls and calls to C stdlib
functions, so you can implement a hello world program in it.

The compiler is implemented in Ruby and is in four stages:

1. Parser produces sexps
2. `Lunacy.build_ast` converts sexps to AST objects
3. AST objects generate IR objects
4. IR objects generate LLVM bitcode

The bitcode is then executed using `lli`. It can also be compiled into a
normal binary executable.

## Example code

This "extended Hello world" works and is in `test.ln`:

    type string = i8*

    extern def printf(string, i32) -> i32
    extern def strlen(string) -> i32

    def main(i32 argc, i8** argv) -> i32
      printf "%d\n", "foobar".strlen
      return 0
    end

## Dependencies

- Ruby 1.9
- `lli`, the LLVM bitcode interpreter