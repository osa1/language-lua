language-lua - Lua 5.2 lexer, parser and pretty-printer
-------------------------------------------------------

[![Build Status](https://travis-ci.org/osa1/language-lua.svg?branch=master)](https://travis-ci.org/osa1/language-lua)

This package is just like any other *language x* packages. It provides lexer, parser and pretty-printer for [Lua](http://www.lua.org/) programming language.

[Haddock documentation](https://osa1.github.io/language-lua)

### Usage

This module provides 3 parsers.

- `chunk`: Lua file parser.
- `exp`: Lua expression parser.
- `stat`: Lua statement parser.

Lexing is needed before running a parser. `parseText` function runs lexer before parsing. So if you want to parse a Lua expression, you can call `parseText exp string`, where string is the Lua expression to parse.

`parseFile` is a helper to parse Lua files. Example: `parseFile "/path/to/lua/file"`. This is same as `readFile path >>= return . parseText chunk`.

Note that `parseText` may result with failure, so it's return type is `Either ParserError a`.

### Lexer

Lexer is not exported by top-level Language.Lua module. You need to import `Language.Lua.Lexer`. After that, `llex string` scans the string and returns token list. Tokens are defined in `Language.Lua.Token`.

### Pretty-printer

Pretty-printer is still under development, and subject to lots of changes. It works, but lots of functionality will be added.

For now, you can use `pprint syntax_tree` to pretty-print a Lua syntax tree.
