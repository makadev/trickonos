

#### Expression Syntax ####

```
  EXPR: NIL | BOOL | INTEGER | STRING | LIST | DICTIONARY |
        NAME_READ | NAME_WRITE | CALL | UNOP | BINOP | 
        INDEX_READ | INDEX_WRITE | ATTR_READ | ATTR_WRITE |
        SCOR | SCAND | METHODCALL | TYPECHECK .
```

#### List Constructor ####

The list constructor (or list builder expression) defines a list object.

Semi-Formal:
```
  LIST   : '[' [ EXPR { ',' EXPR } ] ']' .
```

Informal:
  * A list constructor starts with `[` and ends with `]`, containing one or multiple comma separated expressions
  * list expressions are evaluated from left to right
  * list expressions are equivalent to `[].append(<list of EXPR>)`

**Example**

```
  [];        // valid empty list
  [1];       // valid list with one element (integer object 1)
  [1,2,3];   // valid list with three elements
  [].append(1,2,3); // equivalent to list constructor [1,2,3]
  [[1,2,3],[1,2,3]]; // valid list containing the above list [1,2,3] (twice)

  [;         // invalid, missing ]
  [1 2];     // invalid, not comma separated
```

#### Dictionary Constructor ####

The dict constructor (or dict builder expression) defines a dictionary object.

Semi-Formal:
```
  ASSOC        : (IDENT | STRING) ':' EXPR .
  DICTIONARY   : '{' [ ASSOC { ',' ASSOC } ] '}' .
```

Informal:
  * A dict constructor starts with `{` and ends with `}`, containing one or multiple comma separated associative expressions (ASSOC)
  * dict associative expressions are evaluated from left to right
  * duplicate associative expression keys (part before `:`) are valid, expressions are always evaluated
  * associative expressions are equivalent to either `<dict>.<IDENT> := <EXPR>` for IDENT keys or `<dict>[<STRING>] := <EXPR>` for STRING keys
  * associative expression keys with IDENT keys are added upcasercased

**Example**

```
  {};        // valid empty dictionary
  {id:1};    // valid dictionary, where "ID" is associated integer object 1
  {id:1,"OID":{oid:2}}; // valid dictionary with 2 elements
  
  d := {};           //  d = {}
  d.id := 1;         //  d = {id:1}
  d["OID"] := {};    //  d = {id:1,"OID":{}}
  d["OID"].oid := 2; //  d = {id:1,"OID":{oid:2}}

  {id:1,id:2};   // valid, one assoc (id:2)
  {id:1,"ID":2}; // valid, one assoc (id:2)

  {id:fun(),id:2}; // valid, one assoc (id:2), fun is called

  {;            // invalid, missing }
  {id:1 id:2};  // invalid, not comma separated
  {1:1};        // invalid, key must be ident or string
```

#### Name Read ####

Semi-Formal:
```
  NAME_READ: IDENT .
```

Informal:
  * Reads a named object (assigned global/local variable, parameter or special named object)
  * Scope/Bindings rules are statement (environment) dependent
  * In global scope, this reads the global variable IDENT or returns `nil` if there is no global variable name IDENT

#### Name Write ####

Semi-Formal:
```
  NAME_WRITE: IDENT ':=' EXPR .
```

Informal:
  * Dual to Name Read, it sets a named object
  * Scope/Bindings rules are statement (environment) dependent
  * In global scope, this writes the global variable IDENT, if IDENT was assigned before, the previous value is removed

#### Unary Operators ####

Semi-Formal:
```
  UNOPER: '+' | '-' | 'not' .
  UNOP: UNOPER EXPR .
```

Informal:
  * Unary operation on Object
  * This is equivalent to a Method Call `<EXPR>.<UNOPER>()` thought UNOPER may be implemented internally only (no explicit Method Call alias required), the validity is checked dynamic (runtime) and may return an error or a value

**Example**
```
 nobj := not obj; // Unary operation on Object obj
```

#### Binary Operators ####

Semi-Formal:
```
  BINOPER: '+' | '-' | '*' | 'div' | 'shl' | 'shr' | ... | '=' | '<>' | ... .
  BINOP: LEXPR BINOPER REXPR .
```

Informal:
  * Binary operation on Objects
  * This is equivalent to a Method Call `<LEXPR>.<BINOPER>(REXPR)` thought BINOPER may be implemented internally only (no explicit Method Call alias required), the validity is checked dynamic (runtime) and may return an error or a value

**Example**
```
 thesum := operand1 + operand2; // Binary operation operand1.^Add(operand2)
```

#### Shortcut Binary Operator AND ####

Semi-Formal:
```
  SCAND: LEXPR 'and' REXPR .
```

Informal:
  * Shortcut operation or binary operation
  * LEXPR is always evaluated
  * REXPR evaluation depends on the value of LEXPR (runtime), see table below for input/result for SCAND expression

| **LEXPR VALUE**   | **REXPR VALUE** | **SCAND RESULT** |  **REXPR EVALUATED** |
|:------------------|:----------------|:-----------------|:---------------------|
| l=true          | r             | r              | YES |
| l=false         | r             | l (=false)     | NO  |
| l=nil           | r             | l (=nil)       | NO  |
| l (otherwise)   | r             | (internal) MethodCall `l.^and(r)` | YES |

**Example**

```
 x := a and b;
 // (mostly) equivalent if statement
 if a = nil then
   x := a;
 elseif a = false then
   x := a;
 elseif a = true then
   x := b;
 else
   x := a and b; // <=> binary operation a.^and(b) since a is <> false,true,nil
 end;
```

#### Shortcut Binary Operator OR ####

Semi-Formal:
```
  SCOR: LEXPR 'or' REXPR .
```

Informal:
  * Shortcut operation or binary operation
  * LEXPR is always evaluated
  * REXPR evaluation depends on the value of LEXPR (runtime), see table below for input/result for SCOR expression

| **LEXPR VALUE**   | **REXPR VALUE** | **SCOR RESULT**  |  **REXPR EVALUATED** |
|:------------------|:----------------|:-----------------|:---------------------|
| l=true          | r             | l (=true)      | NO   |
| l=false         | r             | r              | YES  |
| l=nil           | r             | r              | YES  |
| l (otherwise)   | r             | (internal) MethodCall `l.^or(r)` | YES |

**Example**

```
 x := a or b;
 // (mostly) equivalent if statement
 if a = nil then
   x := b;
 elseif a = false then
   x := b;
 elseif a = true then
   x := a;
 else
   x := a or b; // <=> binary operation a.^or(b) since a is <> false,true,nil
 end;
```

#### Object Call ####

Semi-Formal:
```
  ARGS: [ EXPR { ',' EXPR } ] .
  CALL: OEXPR '(' ARGS ')' .
```

Informal:
  * Call (Direct) OEXPR with arguments EXPR
  * Expressions are evaluated from left to right (inclusive OEXPR) before calling
  * Direct Calling is short-form for the (internal) MethodCall `<OXPR>.^Call(<ARGS>)`

**Example**

```
  1(); // valid syntax, but creates a runtime error since Integer doesn't implement ^Call

  function callme; // <- function decl. creates a function object and assigns it to callme
    // do stuff..
    Result := 1;
  end;

  x := callme; // not a call, this simply returns the function object
  x(); // calls the above declared function, functions implement ^Call
  callme(); // also calls the above declared function
  
```

#### Object Method Call ####

Semi-Formal:
```
  ARGS: [ EXPR { ',' EXPR } ] .
  METHODCALL: OEXPR '.' IDENT '(' ARGS ')' .
```

Informal:
  * Call Method IDENT on OEXPR with arguments EXPR
  * Expressions are evaluated from left to right (inclusive OEXPR) before calling

**Example**

```
  x.dosomething(); // valid method call
  x.dosomething; // not a method call (member access, see attribute read)
  x.(y)(); // invalid syntax, method name must be IDENT, not EXPR
```

#### Object Attribute Read ####

Semi-Formal:
```
  ATTR_READ: OEXPR '.' IDENT .
```

Informal:
  * Member getter (special binary operation)
  * Object Attribute Read is short-form for the (internal) MethodCall `<OXPR>.^GetMember(<IDENT>)`

NOTE: _Member access does (in general) nothing with methods, its implementation dependent and a method call itself_

```
  x.some_method_name(); // method call
  x.some_method_name; // <=> x.^GetMember('SOME_METHOD_NAME'), nothing related to the some_method_name method
```

#### Object Attribute Write ####

Semi-Formal:
```
  ATTR_WRITE: OEXPR '.' IDENT ':=' EXPR .
```

Informal:
  * Member setter (special ternary operation)
  * Evaluated left to right (like other calls)
  * Object Attribute Write is short-form for the (internal) MethodCall `<OXPR>.^SetMember(<IDENT>,<EXPR>)`

**Example**

```
  x.someattr := y; // valid syntax, <=> x.^SetMember('SOMEATTR',y);
  
  // dictionary example
  d := {};
  d.id := 1; // add an association as implemented in dictionary type
  d.count := 1; // and another one
  d.count; // = 1
  d.count(); // = 2 -> method call implemented in dict 
```

#### Object Index Read ####

Semi-Formal:
```
  INDEX_READ: OEXPR '[' IEXPR  { ',' IEXPR } ']' .
```

Informal:
  * Index getter (special binary operation wrapper)
  * Evaluated left to right
  * Multiple index expression are handled like multiple index reads, f.e.: `x[1,2]` is equivalent to `(x[1])[2]`
  * Object Index Read is short-form for the (internal) MethodCall `<OXPR>.^GetIndex(<IEXPR>)`

#### Object Index Write ####

Semi-Formal:
```
  INDEX_WRITE: OEXPR '[' IEXPR  { ',' IEXPR } ']' ':=' EXPR .
```

Informal:
  * Index setter (special ternary operation wrapper)
  * Evaluated left to right
  * Multiple index expression are handled like multiple index reads and a single index write f.e.: `x[1,2] := y` is equivalent to `(x[1])[2] := y`
  * Object Index Write is short-form for the (internal) MethodCall `<OXPR>.^SetIndex(<IEXPR>,<EXPR>)`

#### Typecheck ####

Semi-Formal:
```
  TYPECHECK: EXPR 'is' IDENT .
```

Informal:
  * Checks the type name for object evaluated from EXPR and returns Boolean

**Example**

```
  nil is None; // -> true
  nil is Integer; // -> false
  1 is Integer; // -> true
  is func; // syntax error
  false is boolean is BOOLEAN; // -> true
```