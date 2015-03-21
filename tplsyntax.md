

## Template File Structure ##

### Include (Template) and Code (Unit) ###
Trickonos knows 2 types of Template Files.
  * Includes (in general called Template) which is mixed Code and Output much like PHP
  * Code Units (in general called Code/Unit) which is only Code, introduced for template internal declarations, functions, operations and classes


**Example Template**
```
Output Head
{? foreach x in [1,2,3] do ?}
Inner Output {? Write x; ?}
{? end; ?}
Output Tail
```
generates
```
Output Head

Inner Output 1

Inner Output 2

Inner Output 3

Output Tail

```

**Example Code Unit**
```
function write_class(name,funclist);
  WriteLn 'class '+name+' {';
  foreach x in funclist do
    WriteLn('  ',makefunction(funclist),';');
  WriteLn '};';
end;
```
This does not generate code when included with `USE`, but whenever the
function `write_class` is called with corresponding arguments.

NOTE: _Write and WriteLn are macros, see [Core Macros](tplsyntax#Core_Macros.md)_

### Include (Template) Markers ###

In Templates, Code and Output is separated by Markers. The default Markers are:
  * `{?`: start of code
  * `?}`: end of code
Templates always start in non-code Mode, which means everything is Output until
the start of code Marker (also called Code Intro Sequence) is hit. Everything from
the Code Intro Sequence to the next end of code Marker (Code Outro Sequence) is considered code. Code and Output separation is **not arbitrarily** possible. Output is part of the Language and interpreted as statement, those only possible wherever a statement is allowed. see [Statements](tplsyntax#Statements.md)

NOTE: _Template markers may be changed using either the static marker macro or the dynamic System.Marker call_

### Template/Unit Processing Order ###

Trickonos processes files in file order (file by file), not order of statements:

(`thisfile.tpl`)
```
 Do Tricks
 {? INCLUDE 'otherfile.tpl'; ?}
 Do other Tricks
```
The above File (`thisfile.tpl`) is loaded and compiled before `'otherfile.tpl'`.
This creates compile time (**static**) and execution time (**dynamic**) behavior, which must be considered f.e. when changing markers. It allows dynamic on-demand load f.e. loading files mixed with control flow:

(`somefile.tpl`)
```
 {? 
 if booleanvar then 
   INCLUDE 'thisfile.tpl';
 else
   INCLUDE 'otherfile.tpl';
 end;
 ?}
```

  * `somefile.tpl` is loaded and compiled, interpreter starts
  * `thisfile.tpl` is not loaded when `booleanvar<>true` (in fact, this file may not exist), but `otherfile.tpl` will be compiled, loaded and executed in place
  * `otherfile.tpl` same procedure with `booleanvar=true`

NOTE: _booleanvar may not be boolean since trickonos uses an object centric logic, see if/then/else for more_

## Lexical Elements (and Comments) ##

NOTE: _Except for the Withespace/Markers/Code/Non-Code information, this considers only Code Elements_

### (Non Code Elements) Whitespace, Markers, Code/Non-Code ###

NOTE: _This part is for information only_

Semi-Formal:
```
  whitespace      : 0x13 | 0x10 | 0x32 | 0x9 . # (Linefeed, Newline, Space, Tab)
  EOS             : $ . # End Of Stream/File
  CODE_INTRO      : '{?' . # Default Code Intro Sequence
  CODE_OUTRO      : '?}' . # Default Code Outro Sequence
  PUT (non-code)  : ... . # Everything In Non-Code Sections
  NEWLINE         : ... . # System/Settings Dependent Newline Sequence
```

Informal:
  * Whitespaces are either Line Feed, New Line, Space or Tab ASCII Symbols
  * Whitespaces are ignore in Code
  * EOS marks the end of input (Stream or File)
  * CODE\_INTRO marks code start
  * CODE\_OUTRO marks code end
  * NEWLINE is a system dependent line ending sequence (Trickonos detects mixed Unix, Windows and Mac style line endings per default)

### Comments ###

Semi-Formal:
```
  LINE_COMMENT : '//' ... NEWLINE . # single line comment
  ML_COMMENT   : '/*' ... '*/' . # multiline comment 
```

Informal:
  * LINE\_COMMENT is a single line comment, starting with a comment marker (defaults to `//`)
  * ML\_COMMENT is a multi line comment, starting with a ml comment start marker (defaults to `/*`) and ending with a ml comment end marker (defaults to `*/`)
  * Multiline Comments (ML\_COMMENT) can not be nested
  * Comments are (ignored) lexical elements and those can appear before,behind and between any valid code lexical elements

NOTE: _Comment markers can be changed using the static LNMarker, ECCMarker or dynamic System.LNMarker and System.ECCMarker calls_

REMARK: _LN is short fore line, ECC for encapsulated comment (thought its non nestable, may change in future)_

### Identifiers, Value Constructors (boolean, integer, string ..) ###

Semi-Formal:
```
  # WSN like notation for letters/digits
  letter    : 'a' | 'b' | ... | 'z' .
  digit     : '0' | '1' | ... | '9' .
  hdigit    : '0' | '1' | ... | '9' | 'a' | 'b' | .. | 'f' .
  odigit    : '0' | '1' | ... | '7' .
  bdigit    : '0' | '1' .

  # Language Elements
  NIL*       : 'nil' .
  BOOL       : TRUE | FALSE .
  TRUE*      : 'true' .
  FALSE*     : 'false' .
  IDENT*     : ( letter | '_' ) { letter | digit | '_' } .
  INTEGER    : NUMBER | INTHEX | INTOCT | INTBIN .
  NUMBER     : digit { digit } .
  INTHEX*    : '$' hdigit { hdigit } . # freepascal-ish hexadecimal notation
  INTOCT     : '&' odigit { odigit } . # freepascal-ish octal notation
  INTBIN     : '%' bdigit { bdigit } . # freepascal-ish binary notation
  STRING     : STRING_Q | STRING_E .
  STRING_Q   : "'"-quoted-sequence . # double-quote escaped (pascal like) string
  STRING_E   : '"'-quoted-sequence . # escaped (c like) string
```

Informal:
  * Identifiers and Keywords (nil,true,false...) are case insensitive, also some special elements (marked with `*`) like hexadecimal number notation
  * Identifiers (IDENT) start with a letter or underscore optionally followed by one or more (combinations of) letter, digit and underscore
  * Numbers (NUMBER) are sequences of digits
  * Hexadecimal, Octal and Binary Notation is supported using the prefixes `$`, `&`, `%`
  * Numbers in either Decimal (NUMBER), Hexadecimal, Octal or Binary Notation are treated as Integer (NOTE: _minus is an operator, those negative integers are not lexical elements but operations_ )
  * Strings (STRING) are either double-quote (STRING\_Q) or backslash (STRING\_E) escaped strings
  * STRING\_Q is a pascal-ish multiline string, a double-quote sequence will be changed into a single quote sequence: ` 'a''b'  is the same as "a'b"`
  * STRING\_E is a c-ish multiline string, escape sequences will be changed into according characters: ` "\\\"\\" is the same as '\"\' `

#### Escape Sequence ####

The following escape sequences are allowed

| **Escape Sequence**  | **Changed into**              |
|:---------------------|:------------------------------|
| `\\`               | `\`                         |
| `\n`               | Newline Character (`0x10`)  |
| `\t`               | Tab Character (`0x9`)       |
| `\l`               | Linefeed Character (`0x13`) |
| `\"`               | `"`                         |

NOTE: _may be extended in future_

## Core Language ##

NOTE: _Trickonos Language is dynamic typed and object centric. Expressions are very abstract and in general operate on, or produce an object. Statements use object centric logic. Except for value constructors, types are unknown for the compiler and operations simply "try to do something on an object" which results in an error or a valid object._ **The following Documentation is not intended for beginners** _instead, it should be simpler to first have a look at some examples (f.e. the test cases) and/or the [Scripting Objects Documentation](scriptingobjects.md)._

### Expressions ###

#### Expression Syntax ####

See [Expressions](tplexpressions.md)

#### Operator Priorities/Associativity ####

**Operator Table**

| **Op** | **Op Level** (ascending) | **Op Assoc** | **Short Description/General Meaning** |
|:-------|:-------------------------|:-------------|:--------------------------------------|
| `:=` | AEXPR | right | assignment |
| `=`  | RELOP | left | comparision |
| `<>` | RELOP | left | comparision |
| `<=` | RELOP | left | comparision |
| `>=` | RELOP | left | comparision |
| `is` | RELOP | left | typecheck   |
| `+`  | ADDOP | left | add         |
| `-`  | ADDOP | left | substract   |
| `or` | ADDOP | left | logic/binary or |
| `xor` | ADDOP | left | logic/binary exclusive or |
| `*`  | MULOP | left | multiply    |
| `div`  | MULOP | left | divide    |
| `mod`  | MULOP | left | modulo (remainder)   |
| `and` | MULOP | left | logic/binary and |
| `shl` | MULOP | left | (arithmetic) shift left |
| `<<`  | MULOP | left | (shl alias) |
| `shr` | MULOP | left | shift right |
| `>>`  | MULOP | left | (shr alias) |
| `rol` | MULOP | left | rotate left |
| `<<<` | MULOP | left | (rol alias) |
| `ror` | MULOP | left | rotate right |
| `>>>` | MULOP | left | (ror alias) |
| `+`  | UNOP | - (unary) | absolute |
| `-`  | UNOP | - (unary) | negative |
| `not` | UNOP | - (unary) | logic/binary not |
| `.` | SELOP | left | member selector |
| (`(`) | SELOP | left | call |
| (`[`) | SELOP | left | index selector |

**Value Table** (for completeness)

| **Value Type** | **Short Description/General Meaning** |
|:---------------|:--------------------------------------|
| `nil` | nil object |
| `true` | true object |
| `false` | false object |
| `<INTEGER>` | integer constructor |
| `<STRING>` | string constructor |
| `<LIST>` | list constructor |
| `<DICT>` | dict constructor |
| `<IDENT>` | name access |
| `(<EXPR>)` | sub-expression |

**Example**

```
  nil; // nil object
  true; // true object
  false; // false object
  []; // empty list
  {}; // empty dictionary
  ident; // variable/name access
  (true); // sub-expression yielding true

  x := +1.dostuff()() rol 1 + 5 = 2; // <=> x := ((((((+1).dostuff())()) rol 1)+5)=2)

  x := y := z := 1; // <=> x := ( y := ( z := 1 ) )
```


### Statements ###

Trickonos language statements are object driven and can be separated into 2 groups.
  * Core Statements which create a certain control flow using core checks. Core checks means compare and typechecks which are part of the core and those always exist, which implies that these core statements won't fail. As an example, have a look at if/then/else, which relies only on object checks that must be present. Interface dependent statements differ.
  * Interface Dependent Statements which rely on a certain interface (existing methods). As example, have a look at ForEach, which assumes a certain interface and calls methods that may not be present.

In difference to expressions, statements can contain Non-Code passages, which are also interpreted as statements. Trickonos statements are very pascal oriented in sense of look&feel but in difference to pascal, are in general compound statements terminated with _end;_ whereas pascal in general allows a single statement _or_ a compound statement. Without further digging into that topic, there are 2 major reasons for this. First, it resolves problems with the non-code mixing and shortens the language while maintaining the readability of pascal, 2 the language is simpler and contains strict rules about compound/statement terminators and interpretation (f.e. there is no such thing like the dangling else problem and its conform to the strict expression syntax).

Thought, trickonos has _Pascal Compat_ syntax for statements, which allows to write the keyword "begin" in several places, where compound statements start.

Semi-Formal statements:
```
  STAT: IFTHEN | FOREACH | REPEAT | WHILE | PUT | FUNC .
  PUT: non-code lines .
  STATS: ( STAT ) .
```

Informal:
  * STATS is a sequence of statements.
  * STATS may be the empty sequence.
  * PUT consists of non-code.
  * PUT is implementation dependent and may be one ore multiple statements (currently implemented as multiple statements, where each line is a statement)

Semantics:
  * Statements are executed / interpreted in order, from top to bottom (from first to last statement) with exceptions (control flow modifying statements and expressions)

#### If Then ElseIf Else ####

Semi-Formal:
```
  IFTHEN: 'if' <EXPR> 'then' <STATS> (ELSEIF) [ELSE] 'end' ';' .
  ELSEIF: 'elseif' <EXPR> 'then' <STATS> .
  ELSE: 'else' <STATS> .
```

Informal:
  * IFTHEN starts with the keyword 'if', followed by an expression, the keyword 'then' and a sequence of statements, optionally ELSEIF statements, an optional ELSE statement and is finally terminated with the keywords 'end' and ';'
  * ELSEIF starts with the keyword 'elseif', followed by an expression, the keyword 'then' and a sequence of statements
  * ELSE starts with the keyword 'else', followed by a sequence of statements
  * Expressions, also referred to as Conditions, are not limited to Boolean Expressions

Semantics:
  * IFTHEN (and respective ELSEIF) checks - evaluates and tests the resulting object - whether the resulting object (called result) of its Expression is false (Boolean) or nil.
    * If the result is false or nil, the corresponding statements are skipped and the next ELSEIF is tested.
    * If the result is neither false nor nil, the corresponding statements are executed and control flow returns after the 'end' ';'
  * If neither IFTHEN or ELSEIF was executed, and there exists an ELSE, the ELSE statements are executed and control flow returns after the 'end' ';'
  * If neither IFTHEN or ELSEIF was executed and there exists no ELSE then control flow returns after the 'end' ';'

Example _if then_:
```
if true then
  Halt(1);   // this will be executed (always)
end;

if true then // this is allowed
end;

if false then
  Halt(1);   // this is never executed
end;

if nil then
  Halt(1);   // this is never executed
end;

if SomeName then
  Halt(1);   // this depends on the value of SomeName
end;

if (SomeName <> nil) and 
   (SomeName <> false) then // semantically equivalent to the above
  Halt(1);
end;

if x := nil then // allowed
  Halt(1);       // never executed since expression = nil
end;
```

Example _if then elseif_:
```
if false then
  Halt(1);        // not executed
elseif true then
  Halt(2);        // always executed
end;

x := 2;
if (y := x) < 1 then      // assigned, but if is skipped
elseif (z := x) = 2 then  // assigned, elseif is executed, control flow returns on end
elseif (a := x) = 3 then  // not assigned, since this part was skipped
end;
```

Example _if then elseif else_:
```
if true then // control flow returns on end, both elseif/else are skipped
elseif false then
  Halt(1);
else
  Halt(1);
end;

if false then
  Halt(1);
else
  Halt(0); // always executed, program stops with 0
end;
```

#### Repeat Until ####

Semi-Formal:
```
  REPEAT: 'repeat' <STATS> 'until' <EXPR> ';' .
```

Informal:
  * REPEAT starts with the 'repeat' keyword, followed by statements, the 'until' keyword, an expression and terminates with ';' .
  * Typical foot controlled loop, where Expression is tested after execution of statements, those statements are executed at least once

Semantics:
  * REPEAT tests the expression result for nil and false.
    * If the expression result is nil or false, the loop will be reentered on the first statement of STATS
    * If the expression result is neither nil nor false, the loop is terminated and control flow returns after ';'
  * REPEAT may result in an endless loop

Example:
```
  x := 1;
  repeat
    x := x + 1; // increment x by 1
  until x > 10; // terminate loop, when x > 10

  y := nil;
  repeat
    x := x + 1; // increment x by 1
    if x > 10 then // assign x to y when x > 10
      y := x;
    end;
  until y; // terminates loop, when y is assigned

  y := nil;
  repeat
  until y; // endless loop, since y is always nil
```

#### While Do ####

Semi-Formal:
```
  WHILE: 'while' <EXPR> 'do' <STATS> 'end' ';' .
```

Informal:
  * WHILE starts with the 'while' keyword, followed an expression, the 'do' keyword, statements and terminates with 'end' ';' .
  * Typical head controlled loop, where Expression is tested before execution of statements, those statements may never be executed

Semantics:
  * WHILE tests the expression result for nil and false.
    * If the expression result is neither nil nor false, the loop will be reentered on the first statement of STATS
    * If the expression result is nil or false, the loop will be terminated and control flow returns after 'end' ';'
  * WHILE may result in an endless loop

Example:
```
  x := 1;
  while x < 10 do // terminate loop, when x > 10
    x := x + 1;   // increment x by 1
  end;

  y := nil;
  while y do    // terminate when false/nil
    y := x + 1; // never executed 
  end;

  y := 1;
  while y do   // endless loop, since y <> nil and y <> false
    x := x + 1;
  end;
```

#### Function Declaration ####

Semi-Formal:
```
  FUNCARG: <NAME> [ ':=' <EXPR> ] | 'varargs' [ ':=' <EXPR> ] .
  FUNCARGS: '(' [ <FUNCARG> (',' <FUNCARG>) ] ')' .
  FUNCVARS: 'var' <NAME> [, <NAME> ] ';' .
  FUNC: 'function' <NAME> [ <FUNCARGS> ] ';' [<VAR>] <STATS> 'end' ';' .
```

Informal:
  * A function declaration starts with the keyword 'function', optionally followed by a list of function parameters, the head terminator ';', optionally a list of variables, a list of statements and the body terminating keywords 'end' ';' .
  * A list of variables is a simple list of names.
  * A list of parameters is a simple list of names, where names may be followed by ':=' and an initializing (_default_) expression.
  * Parameters with default expression must be behind those without default expressions, f.e. `function a(b := 1, c); ..` syntactically incorrect. An exception is the name 'varargs' which must be last. varargs may too have a default expression, but it is not required.
  * Parameter names, the function name, variables and the special name 'result' are variable names in the function. Both function name and 'result' may be reused as either variable (in 'var' part) or parameter, f.e. `function a(a,result); ..` is correct. But, duplicate variable and parameter names are incorrect.

Semantics (declaration, function objects, calling):
  * Whenever a function **declaration** is evaluated, it registers a function object with the function name in the current scope.
  * Function objects are anonymous, the name serves simply the purpose of storing the function object for both declaration and calling (and self calling/recursion)
  * A **call** (see expressions) on function object, enters the function with given arguments, where:
    * For all **parameters without default expression**, there must be an argument which is set in the corresponding parameter name
    * For all **parameters with default expressions**, there may be an argument which is set in the corresponding parameter, if there is no argument the default expression will be evaluated and the value set in that variable
      * Default expression are evaluated on call, not on declaration and those depend on the environment at call time
      * Default expressions are evaluated in parameter passing order (left to right)
        * Default expressions are allowed to access other parameters to the left, f.e. `function a(b := 1, c := b); ...` assigns 1 to b and local variable b to c
        * Default expressions can not access the current name and those to the right, 'result' or the function name, these names will read from the global environment
    * If the call passes more arguments then actual parameters (varargs name is not accounted as such) exist, every argument not matching a parameter is put into a list set into **varargs** (called vararg parameters)
      * varargs is either a list if there are vararg parameters **or**
      * nil if there are no vararg parameters **or**
      * a default value if there are no vararg parameters an a default expression was specified
    * All **names** used in a function access the global scope except for those registered as variables, the function name, 'result' and parameter names. These are in the functions scope
      * Variables are **initialized** with nil
      * In the function body, all variables can be read and written without restrictions.
      * **Result** is initialized with nil, when not used as parameter or function name
      * Result is special, as its the name (or variable) used to return infos to the caller (return value).
      * The **function name** contains the function object for self calling (recursion) when not used as parameter or variable name
  * The function is left (return) after the last statement and result is passed back as return value (function result)

Example simple fun:

```
  function simple;
    Result := 'simple';
  end;
  
  writeln simple; // outputs: function object infos
  writeln simple(); // outputs: simple
```

Example parameter:

```
  function sum(a,b);
    Result := a+b;
  end;

  x := sum(1,2);   // -> x = 3
  x := sum(1);     // error, wrong arg num
  x := sum(1,2,3); // error, wrong arg num
```

Example default parameter:

```
  function sum(a,b,c := 0);
    Result := a+b+c;
  end;

  x := sum(1,2);   // -> x = 3
  x := sum(1);     // error, wrong arg num
  x := sum(1,2,3); // -> x = 6

  function sum2(a:=1,b,c); // error -> b/c missing default expressions
  end;
```


Example varargs:

```
  function sum(a,b,varargs);
    Result := a+b;
    if varargs then
      foreach x in varargs do   // <- x is global!
        Result := Result + x;
      end;
    end;
  end;

  x := sum(1,2);   // -> x = 3
  x := sum(1);     // error, wrong arg num
  x := sum(1,2,3); // -> x = 6


  function better_sum(varargs := []);
  var x;
    Result := 0;
    foreach x in varargs do   // <- now, x is local
      Result := Result + x;
    end;
  end;

  sum := better_sum; // replace old fun sum with better sum

  x := sum(1,2);     // -> x = 3
  x := sum(1);       // -> x = 1
  x := sum(1,2,3);   // -> x = 6
  x := sum(1,2,3,4); // -> x = 10
```


TODO: some more, variable usage, sub functions

#### ForEach In Do ####

TODO

## Compiler Macros (Language Extensions) ##

### Core Macros ###

TODO (write`*`, halt, assert, include, use)

### Extension Macros ###

TODO (output handling macros ...)