# S-expression syntax

## Notation conventions

In notation below, literal characters are given in **bold** while placeholders
in _italic_ are described by further grammar rules. Repetitions are written
as \[ _x_ ... \] which indicates zero or more repetitions of the construct of
kind _x_.

## Syntax

Each s-expression is one of either:

- a _literal_ s-expression
- a _compound_ s-expression

A _literal_ s-expression is a string without whitespace or parentheses (or
has these special characters quoted by backslash).

A _compound_ s-expression has the form

**(** _optional-whitespace_ _kind_ \[_whitespace_ _arg_ \[_whitespace_ _arg_ ...\] \] _optional-whitespace_ **)**

where

- **(** and **)** are literal parentheses characters
- _whitespace_ is one or more whitespace characters (space, tab, cr, lf)
- _optional-whitespace_ is zero or more whitespace characters
- _kind_ is a string in the same form as _literal_
- each _arg_ is itself an s-expression

# Coq AST s-expressions

## <a name="term">Term s-expressions (_term_) </a>

A _term_ s-expression is one of either:

- [Sort](#sort)
- [Global](#global)
- [Local](#local)
- [Dependent product](#prod)
- [Lambda](#lambda)
- [Let](#let)
- [Apply](#app)
- [Fix](#fix)
- [Cofix](#cofix)
- [Int](#int)
- [Float](#float)

XXX: There are a few more possible constructs that are not used anywhere in
the code.

### <a name="sort">Sort</a>

(**Sort** _sortname_)

Where:

- _sortname_ is a literal string designating the name of the sort (Set, Prop
  or Type); designates the "type" of other "types" in Coq's type system

### <a name="global">Global</a>

(**Global** _name_)

- _name_ is a literal string identifying the referenced global

### <a name="local">Local</a>

(**Local** _name_ _index_)

- _name_ is a literal string used for the name -- it is _not_ guaranteed to be
  unique and exists as convenience, use _index_
- _index_ is a literal string giving a non-negative integer; it identifies the
  referenced local by [de Bruijn index)[#debruijn]

### <a name="prod">Dependent product</a>

(**Prod** _argname_ _argtype_ _valtype_)

- _argname_ is of type [argname](#argname) and names the argument; it is pushed
  as [de Bruijn index](#debruijn) for interpretation of _valtype_
- _argtype_ is of type [term](#term) and identifies the type of the argument
- _valtype_ is of type [term](#term) and identifies the type of the value

### <a name="lambda">Lambda abstraction</a>

(**Lambda** _argname_ _argtype_ _body_)

- _argname_ is of type [argname](#argname) and names the argument; it is pushed
  as [de Bruijn index](#debruijn) for interpretation of _body_
- _argtype_ is of type [term](#term) and identifies the type of the argument
- _body_ is of type [term](#term) and gives the body of the function

### <a name="let">Let</a>

(**LetIn** _name_ _termval_ _termtype_ _body_)

- _name_ is of type [argname](#argname) and names the argument; it is pushed
  as [de Bruijn index](#debruijn) for interpretation of _body_
- _termval_ is of type [term](#term) and gives the term assigned to the new term
- _termtype_ is of type [term](#term) and identifies the type of the term
- _body_ is of type [term](#term) and gives the body which may use this let
  assignment

### <a name="app">Apply</a>

(**App** _fn_ _arg_)

- _fn_ is of type [term](#term) and identifies the function
- _arg_ is of type [term](#term) and identifies the argument to be applied to _fn_

### <a name="case">Case</a>

(**Case** _nargs_ _casetype_ _match_ _branches_)

- _nargs_ is a literal string giving a non-negative integer; it describes the
  number of arguments in the match (NB this is 1 at the moment)
- _casetype_ is of type [term](#term) and gives the type of the entire match
  expression dependent on its argument -- that means, this is always a _lambda_
  abstraction, and applying it to the term under case analysis yields the
  "result type" of the entire match expression
- _match_ is of type [match](#match) and describes the match argument
  (i.e. the term under case analysis)
- _branches_ is of type [branches](#branches) and describes the branches /
  patterns that the term is matched with

#### <a name="match">Match</a>

(**Match** _expr_)

- _expr_ is of type [term](#term) and gives the term to be matched

#### <a name="branches">Branches</a>

(**Branches** \[_branch_ ...\])

- _branch_ is of type [branch](#branch) and gives an individual pattern matching
  branch

#### <a name="branch">Branch</a>

(_consname_ _nargs_ _expr_)

- _consname_ is a literal string giving the name of the constructor that is
  matched against -- this takes the form of a fully qualified global reference
- _nargs_ is a literal string giving a non-negative integer, it describes the
  number of arguments of the constructor
- _expr_ is of type [term](#term) is an expression to which the constructor
  arguments can be applied and that then yields the match result for this branch

### <a name="fix">Fix</a>

(**Fix** _index_ _fixfn_ \[_fixfn_ ...\])

- _index_ is a literal string giving a non-negative integer; it describes the
  index of the function -- the expression as a whole will have the type
  signature and behavior of the function identified by this index
- _fixfn_ is of type [fixfn](#fixfn) and describes the functions; the names
  of all functions are added in their order of definition to the context of
  interpretation for the _def_ of all functions

#### <a name="fixfn">Fixfn</a>

(**Function** _name_ _sigtype_ _def_)

- _name_ is a literal string giving the local name of this function; this name
  usable in the _body_ of all peer fixpoint functions
- _sigtype_ is of type [term](#term) and designates the type signature of this
  function (i.e. this will be a [dependent product](#prod))
- _def_ is of type [term](#term) and gives the definition of the function
  (i.e. this will be a [lambda](#lambda) matching the type of _sigtype_.

### <a name="cofix">Cofix</a>

undocumented / unused at present (only relevant for co-inductive types
which are not supported at present either)

### <a name="int">Int</a>

(**Int** _value_)

- _value_ is a literal string that represents an integer

### <a name="float">Float</a>

(**Float** _value_)

- _value_ is a literal string that represents a floating point value

## <a name="argname">Argument name (_argname_)</a>

An _argname_ s-expression is one of either

- [named argument](#named)
- [anonymous argument](#anonymous)

### <a name="named">Named argument</a>

(**Name** _id_)

- _id_ is a literal string supplying a name (for a local variable or argument)

### <a name="anonymous">Anonymous argument</a>

(**Anonymous**)

Supplies an argument name where one is syntactically required, but no actual
name is needed (e.g. because the argument/variable is unused). Note that
a [de Bruijn index](#debruijn) will still be reserved even for anonymous
arguments / variables

# <a name="debruijn">de-Bruijn indices</a>

Local variables (and function arguments) are referenced by the _de-Bruijn_
index. From any given expression, the innermost variable always has index 0,
the next outer has index 1 etc. For example, in this s-expression:

**(\(LetIn foo)** _val_ _type_ **(\(LetIn bar)** _val_ _type_ _body_**\)**

In the context of _body_, the local variable **bar** will have index 0,
the variable **foo** will have index 1. Each further nesting of local
definitions will shift existing indices upwards. Note that the augmented
context with the new definition may only be applied to _some_ of the
child expressions -- in the case at hand, **bar** exists in the context of
_body_, but not in the context of _val_ and _type_ and this is also reflected
in de-Bruijn numbering.
