

# Introduction #

Collective page for development items (TODO's).

Every development item should consider:
  * **Milestone**
  * **Reason for Implementation** (one of the [key requirements](tcrequirements.md))
  * **Priority**
  * **Severity** (complexity)
  * **Description** (detailed Reason and Gain, also problems that may occur)
  * **Design(s) and Implementation(s)** (one or multiple possible implementations (rough))
  * **Possible Tests**

Workflow:
  * (getting hit by an Apple)
  * Consider Reason and Gain (is there nothing equivalent? does it increase performance/security/functionality/maintainability/...? )
  * Consider Complexity (high complexity should be justified by high gain)
  * Document (design, rough implementation plans)
  * Review Design (complexity/gain/reason changes?)
  * Start Implementation
    * Create a Ticket(s)
    * Always branch for big extensions/modifications (allows to drop it if its not worth the effort/introduces more problems then gain)
    * Write Tests (if possible, otherwise consider implementing a test strategy)
  * Review Implementation (may start redesigning and so on)
  * Commit when done, master should always be compilable and succeed for all tests (Incremental Workflow)
  * fix/extend Documentations

# List of TODOs #

## Language Extension (Break,Continue,Exit) ##

| **Milestone** | 0.2a alpha extension |
|:--------------|:---------------------|
| **Reason**    | functionality |
| **Priority** | high |
| **Severity** | Medium`*` |

`*`: _Most code is already implemented, loops/function frames are tracked compiler side but the code is mostly untested so depending on the correctness of current implementation this ranges from 'quick introduce compiler side handling' (trival) to 'rewrite the frame handling' (high)_

### Description ###

Break, Continue loop control, Exit function/template control. Gain is compatibility with other languages, functionality and usability, also part of performance.

### DaI ###

Current compiler frame stack tracks loop exit/reintro and exit for functions. Structures that utilize the stack for temporary values (foreach) are a bit tricky for break, since it needs to insert pops to even frame before exiting these structures. Exit is quite simple, since the vm traces framepointers before call/template push, so exit simply needs to jump at the end and ret/template pop will cleanup the frame.

function exit may return a value (replace a passed value with current result slot in function frame) while template exit raises an error for passed argument.

### Tests ###

  1. succeed, compile: break/continue/exit
  1. succeed, controlflow: break/continue in while w/o function frame
  1. succeed, controlflow: break/continue in repeat w/o function frame
  1. succeed, controlflow: break/continue in foreach w/o function frame (should fail when stack uneven)
  1. succeed, controlflow: break/continue in 2 loops (inner/outer)
    * at least 2 times foreach
    * mix of repeat/while is possible, they are quite simple
  1. succeed, controlflow: exit plain (first template)
  1. succeed, controlflow: exit in included template
  1. succeed, controlflow: exit from function
  1. succeed, controlflow: exit from sub-function
  1. fail, statement: break/continue without loop

## Language Extension (For .. To .. Do) ##

| **Milestone** | unknown |
|:--------------|:--------|
| **Reason**    | functionality (partial compatibility/usability with other languages) |
| **Priority** | Medium |
| **Severity** | Medium |

### Description ###

Typical Programming Language Construct. Gain is mostly compatibility/usability/functionality, depending on implementation may improve performance (counter object preserved on stack, probably no variable lookups needed) slightly.

### DaI ###

Currently, multiple designs are considered.

**Example** (Integer)
```
  'for' <IDENT> := <Start EXPR> ('to'|'downto') <Term EXPR> [step <Step EXPR>] 'do'
     <STATS>
  'end' ';'


  cnt := <IDENT> := eval(<Start EXPR>);
  term := eval(<Term EXPR>);
  step := eval(<Step EXPR>);
looptest:
  if cnt <= term then // for to, >= for downto
    cnt.Lock;
    eval <STATS>;
    cnt.Unlock;
    cnt := <IDENT> := cnt + step; // for to, - for downto
    goto looptest;
  else
    goto exit;
exit:
```

other possibility 1: merge for to/downto with foreach and use some kind of range (object) which is iterated: needs review of iterators, probably new iterator types

other possibility 2: more flexible (c-ish) for with start expression, update expression (defaults to cnt+1/cnt-1 for to/downto) and termination condition (needs reevaluation... every loop): quite simple implementation since its mostly like while loop, basic problem is the question, if the counter should be locked (secured against modifications) or not which is harder the more flexible the construct is.

### Tests ###

N/A

## Multiple Type Functionality Extensions ##

| **Milestone** | 0.2a alpha extension |
|:--------------|:---------------------|
| **Reason**    | functionality |
| **Priority**  | High |
| **Severity**  | Trivial but time consuming |

### Description ###

0.1a Implementations are basic (proof of concept, implementation of extendable system).
Typical operations must be implemented for functionality and documented.
Also several operations that may be simply implemented in script but are much faster in native code.

#### Boolean ####

  * `tostr([true_str: string, false_str: string]) -> string` (mapping boolean to strings)
  * `toval(true_val: any, false_val: any) -> any` (mapping boolean to any value)

#### Integer ####

  * `tostr() -> string` (string conversion)
  * `tohexstr([digits: integer]) -> string` (hexadecimal string with at least digits positions)
  * `tooctstr([digits: integer]) -> string` (octal string with at least digits positions)
  * `tobinstr([digits: integer]) -> string` (binary string with at least digits positions)

#### String ####

  * `trim() -> string` (remove white-spaces both sides, implemented, not documented)
  * `ltrim() -> string` (remove white-spaces from start, implemented, not documented)
  * `rtrim() -> string` (remove white-spaces from end, implemented, not documented)
  * `split(delim: string) -> list of string` (split into list of strings using delimiter delim)
  * `join(strings: list of string) -> string` (join string list using self as delimiter, implemented, not documented)
  * `join(str: string,[varargs of string]) -> string` (same as join, but join arguments directly)
  * `length() -> integer` (get string length, implemented, not documented)
  * `pos(substr: string) -> integer` (get position of first match for substring)
  * `posex(start: integer, substr: string) -> integer` (get position of first match for substring starting at start)
  * `copy([start: integer, [length: integer]]) -> string` (copy part of string, starting at start or first character, copy length characters or til end)
  * `lowercase() -> string` (return lowercased string)
  * `uppercase() -> string` (return uppercased string)
  * `delete(start: integer, [length: integer]) -> string` (delete part of string, starting at start, deleting length characters or til end)
  * `dup(count: integer) -> string` (append string count times (`'a'.dup(2) = 'aa'`))
  * `charlist() -> list of string` (convert into list of characters (strings of length 1) **memory inefficient but faster then index getter multiple times**)
  * `replace([strs: list of list of strings]) -> string` (replace one or multiple substrings in string, f.e. `'abc'.replace([['a','b'],['b','c']]) = 'bcc'`)
  * `replace([varargs of list of string]) -> string` (replace using varargs)
  * `format([fmtlst: list of any]) -> string` (format string)
  * `format([varargs of any]) -> string` (format string)
  * `subst(subs: dict) -> string` (substitute sub-strings)

Note: _should check if its really needed to create new strings for each operation or simply modify self which should improve performance slightly but may be unexpected_

#### List ####

Note: _time consuming operations should operate on self, since copying a list again and again is real performance killer_

  * `append(item: any,[varargs of any]) -> self` (implemented (needed by core features), not documented)
  * `prepend(item: any,[varargs of any]) -> self` (prepend items)
  * `appendlist(list:list of any) -> self` (append a list)
  * `prependlist(list:list of any) -> self` (prepend a list)
  * `copy([start: integer, [length: integer]]) -> list` (**flat** copy)
  * `iterator() -> ios_list_iterator` (create iterator, implemented (needed by core features), not documented)
  * `delete(start: integer, [length: integer]) -> self` (delete part of list, starting at start, deleting length items or til end)
  * `clear() -> self` (same as `delete(1)`)
  * `reverse() -> self` (reverse list)
  * `length() -> integer` (get length, implemented, not documented)
  * `isempty() -> boolean` (check if list is empty)
  * `first() -> any` (first element, may return nil if empty)
  * `last() -> any` (last element, may return nil if empty)
  * `popfront() -> any` (remove and return first, may return nil if empty)
  * `popback() -> any` (remove and return last, may return nil if empty)

#### Dictionary ####

  * `copy([keys: list of string]) -> dict` (copy whole or only keys given in keys)
  * `copy([varargs of string]) -> dict` (copy using vararg keys)
  * `delete(key: string, [varargs of string]) -> self` (delete one or multiple keys)
  * `clear() -> self` (delete all assocs`)
  * `count() -> integer` (implemented, not documented)
  * `keys() -> list of string` (create a key list)
  * `items([keys: list of string]) -> list of any` (create a item list for whole dictionary or specific keys given by list)
  * `items([varargs of string]) -> list of any` (same as items using varargs)
  * `exists([keys: list of string]) -> boolean` (check for existence of one or multiple keys)
  * `exists([varargs of string]) -> boolean` (same as exists using varargs)


## Type Extension (array) ##

| **Milestone** | 0.2a alpha extension |
|:--------------|:---------------------|
| **Reason**    | performance/functionality |
| **Priority** | Medium |
| **Severity** | type implementation: Low, type methods dai: trival+massive |

### Description ###

Implement an array type. Consider replacing varargs/listbuilder and some other internals with arrays (since stack is already an array, so moving around pointers may be much faster then allocation of list elements/building lists per list creation and vararg passing).

Instead of replacing, maybe its better to introduce a new 'vector' builder. f.e.
```
  somearray := <item,item>;
  
  somecall(<1,2,3>); // thought, this one looks quite ugly
```

### DaI ###

Analogous to current list implementation, replacing internal list with an array. Nothing special.

### Tests ###

N/A