Parsing ABC Tunes with `rly`
================
Curtis C. Bohlen
11/14/2021

-   [Introduction](#introduction)
    -   [The Structure of `rly`](#the-structure-of-rly)
    -   [R6 Classes and `rly`](#r6-classes-and-rly)
-   [Load libraries](#load-libraries)
-   [Test Data](#test-data)
-   [A Simple Tune Lexer](#a-simple-tune-lexer)
    -   [The Lexer](#the-lexer)
    -   [What’s in a Lexer?](#whats-in-a-lexer)
        -   [Methods](#methods)
        -   [Members](#members)
-   [A Simple Tune Parser](#a-simple-tune-parser)
    -   [A `note` Class](#a-note-class)
    -   [Specifying the Parser](#specifying-the-parser)
-   [Testing the Parser](#testing-the-parser)
-   [Other R Parsing Engines](#other-r-parsing-engines)
-   [R Packages that simplify Tree Data
    Structires](#r-packages-that-simplify-tree-data-structires)
-   [A Formal Grammear](#a-formal-grammear)

# Introduction

In this notebook, I explore the R parsing engine `rly`. `rly` is a port
to R of a python package, `ply`. The name `ply` implies “python lex and
yacc”, where “lex” and “yacc” are classic UNIX tools for building formal
parsers.

Unfortunately, the documentation for `rly` is sparse, and there are few
examples available on-line, so it takes some time to understand how to
work with the package. One can get some idea of how everything works by
digging into the package’s precursors, especially `ply`, “lex” and
“yacc”, which are better documented, but details of usage in R are often
implied, not clearly explained.

This Notebook documents “lessons learned” by developing simplified
parsers for reading simplified ABC music files.

## The Structure of `rly`

The package `rly`, as an R implementation of “lex” and “yacc”, provides
tools for implementing both a “lexer” and a “parser”.

You construct a lexer by calling the function
`rly::`lex()`.  You create a  parser by calling the function`rly::yacc()\`.

To parse an input string, you apply the lexer and the parser you just
produced to the input.

## R6 Classes and `rly`

The package is built in R via the R6 object oriented system (see Chapter
14 of Hadley Wickham’s *Advanced R* for a primer on this flavor of
object oriented programming in R).

R6 provides a form of object oriented programming that is a lot like
python’s class-based object oriented programming style. An R6 Class
encapsulates both methods (functions) and attributes (data).

In R6, creating an R6 object occurs in two steps – just like in python.
First, you have to create an R6 Generator, then you produce actual R6
objects as instances of the class using that generator. This is exactly
analogous to how you generate an object of a specific class in python.
First you define the Class, then you instantiate that class by calling
the Class constructor.

R6 objects turn up a lot in `rly`. The user designs lexers and parsers
by constructing R6 objects to pass to `lex()` and ’yacc()\`. The objects
those two functions produce are themselves R6 objects, and (at least by
default), many of the underlying data structures are R6 objects as well.

# Load libraries

I keep dependencies to a minimum here, so examples can be applicable in
more settings. I make light use of the tidyverse for data manipulation,
(`dplyr`) and little moree.

The only reason we use the `abcr` package is to access a couple of tune
examples.

``` r
#library(tidyverse)
#library(dplyr)
#library(tibble)
library(abcr)
#library(stringr)
library(rly)
#library(data.tree)
```

# Test Data

``` r
arpeggios_1 <- "CEG2 CFA2 DGB2"

arpeggios_2 <- "GBD2 GCE2 AD^F2"   # Add a Sharp

arpeggios_3 <- "G_BD2 GC_E2 ADF2"   # Add two flats

arpeggios_4 <- "GBd2 Gce2 Ad^f2"   # Add variation in octaves

arpeggios_5 <- "GBD'2 GC'E'2 AD'^F'2"   # octave modifiers should produce same as last

arpeggios_6 <- "g,b,d2 g,ce2 a,d^f2"   # octave modifiers should produce same as last
```

# A Simple Tune Lexer

A Lexer accepts a stream of text input, and identifies discrete tokens.
Each token is a molecule of meaning that you may want to interpret
further as you parse the text input.

This first lexer takes a string of ABC Notes, with or without white
space, and converts it to a sequence of tokens, in order. Not verty
exciting.

Each token is defined either as a literal, via a REGEX, or via a
function. The choice of what to code each way has implications for
precedence, but for now, we only use REGEX expressions.

We start by producing a lexer that can handle a very reduced form of
ABC: a string of notes,

## The Lexer

ABC notes are basically letters indicating pitch names from A to G. To
code the two octaves beginning with middle C, which correspond to melody
lines in a lot of folk music, Those letters can be capitalized (Octave
4) or not (octave 5). Those bare pitch names can be decorated with other
information, especially with one or more leading sharp or flat
indicators, one or more trailing octave adjustment indicators, and a
following number that indicates the note duration (usually in eighths).

In constructing the R6 object that will produce the parser, we define a
list of tokens, a list of literals, and specially-named REGEX strings or
functions, each of which begin with a “t-”. The order of functions
matters. Strings caught in earlier REGEX don’t get caught by later ones.
Only items that “fall through” all prior REGEXes hit `t-error()`.

``` r
TOKENS = c('ACCIDENTAL', 'BASENOTE', 'OCTAVE_ADJ', 'DURATION', 'WHITESPACE')
LITERALS = c()

SmallLexer <- R6::R6Class("Lexer",
  public = list(
    tokens = TOKENS,
    literals = LITERALS,
    t_ACCIDENTAL = '[\\^=_]',
    t_BASENOTE = "[a-gA-G]",
    t_OCTAVE_ADJ = "[\\',]+",
    
    t_DURATION = function(re='\\d+', t) {
         t$value <- strtoi(t$value)
         return(t)
    },
    t_WHITESPACE = "\\s+",
    
    # we Need a Function to Increment Line Numbers, to elp withe fixing errors
    t_newline = function(re='\\n+|(?:\\n\\r)+', t) {         
      t$lexer$lineno <- t$lexer$lineno + nchar(t$value)
      return(NULL)
    },
    
    # define an error function for unrecognized tokens or characters,
    # here we simply skip to teh next character and try again.
    t_error = function(t) {
      cat(sprintf("Illegal character '%s'", t$value[1]))
      t$lexer$skip(1)
      return(t)
    
    }
  )
)

smalllexer <- rly::lex(SmallLexer)
```

We use a parser by feeding it some input (with the `$input()` method)
then pulling tokens out one at a time(with the `$token()` method).

``` r
smalllexer$input(arpeggios_1)
while (!is.null(current_token <- smalllexer$token())) {
  print(current_token)
}
#> LexToken(BASENOTE,C,1,1)
#> LexToken(BASENOTE,E,1,2)
#> LexToken(BASENOTE,G,1,3)
#> LexToken(DURATION,2,1,4)
#> LexToken(WHITESPACE, ,1,5)
#> LexToken(BASENOTE,C,1,6)
#> LexToken(BASENOTE,F,1,7)
#> LexToken(BASENOTE,A,1,8)
#> LexToken(DURATION,2,1,9)
#> LexToken(WHITESPACE, ,1,10)
#> LexToken(BASENOTE,D,1,11)
#> LexToken(BASENOTE,G,1,12)
#> LexToken(BASENOTE,B,1,13)
#> LexToken(DURATION,2,1,14)
```

Each LexToken is itself an R6 object, with a print method that hides
some details of the R6 implementation. The visible contents are (in
order):  
\* Token TYPE \* TOKEN VALUE \* Line number (always 1 if you don’t
manage the line count yourself.) \* Position in the source text (NOT
character count, but token count).

A full list of methods and attributes is accessible because R6 is built
on top of r environments. We find methods `$clone()`, `$print()`, and
`$toString()`.

``` r
smalllexer$input(arpeggios_1)
(tok <- smalllexer$token())
#> LexToken(BASENOTE,C,1,1)
ls(tok)
#> [1] "clone"    "lexer"    "lexpos"   "lineno"   "parser"   "print"    "toString"
#> [8] "type"     "value"
```

Attributes are accessible too. The ones shown in the display, which are
the ones most users may want to manipulate, are accessible via
attributes `$type`, `value`, `lexpos`, and `lineno`. But there is no
error checking, so you can easily generate a meaningless token.

``` r
tok$lineno <- 7
tok$lexpos <- 17
tok$type = 'SLOPPY!'
tok
#> LexToken(SLOPPY!,C,7,17)
```

Two members of the class that LOOK useful are often NULL. presumably,
these are only instantiated during actual lexing or parsing. Not sure
howe that magic is accpmplished.

``` r
tok$lexer
#> [1] NA
tok$parser
#> [1] NA
```

## What’s in a Lexer?

We use a little introspection to understand the structure of a Lexer,
created from a Lexer Generator, with the function `lex()`.

### Methods

According to the man pages for `rly::Lexer`, each lexer only has a few
public methods. These include:

input() - Store a new string in the lexer

token() - Get the next token

clone() - Clone the lexer (shallow clone by default)

We’ve seen how to use both `input()` and `token()` already. `clone()`
makes a copy of the Lexer. Since you can reuse a lexer simply, the clone
method is likely most useful if you want to copy and modify an existing
lexer.

There are actually a few more methods visible via introspection, but
they are not intended for external use.

### Members

The Man pages suggest only a couple of the attributes of a Lexer are
intended for public use:

lineno: Current line number

lexpos: Current position in the input string

Numerous other attributes are used by the lexer internally to manage the
lexing process.

# A Simple Tune Parser

Next, we build a simple parser to take the output from a lexer and turn
in into something more useful. Our goal here is some sort of data
structure that can be converted into useful forms, such as a data frame
of notes or a musicXML file. Here we focus on generating a list of
notes, where a note contains complete pitch and duration information.

The general steps are similar to how we produced a lexer: we have to
define the structure of a parser with a combination of tokens, literals,
and parsing functions, defined by formal grammar.

(The notion of a formal language, and the many flavors of parsers are
well beyond the scope of this Notebook.)

The rules of the parser here take on a form that was inherited from the
original `yacc` program, and redefined in python’s `ply`.

Each rule combines a grammar rule that expresses how to combine tokens,
with code (in R) that is run each time that particular rule is invoked
during parsing.

The rules are embodied in functions with names that start with ‘p\_’.
Each function must contain a parameter `doc`, with a default string that
defines the parsing rule. The function also takes a second argument,
conventionally named `p`. The body of the function is the action to take
(usually to modify p) when that rule is invoked. (The name “doc” derives
from the Python implementation, which located the parsing rules in the
“docstring” associated with each function.)

The function parameter `p` represents a “YaccProduction” R6 object.
Documentation on the nature of this object is sparse. The class is a
wrapper around the objects actually passed to each grammar rule.

The primary responsibility of each function is to transform the values
of P, which are then passed on to the next step in the parser, and so
on. If the input string is successfully parsed, the parser returns the
final value of p, usually some sort of useful data structure.

Each function can, in principal, also alter external data structures,
set flags, etc. That may be useful to alter behavior of the parser,
store variable names, etc.

## A `note` Class

We want a consistent interface to underlying data types here. Most
important is the concept of a NOTE, which consists of the combination of
accidentals, a note name, an octave specification, and a duration.

We could construct an R6 class for this purpose, which is quite natural,
since we are working with R6 classes already, but R6 is not very much in
keeping with R idioms.

The alternative is to ‘build’ an S3 class. But S3 “class” is all about
method dispatch, so what we would need clarity on what methods would
simplify coding the parser.

The main thing we would want for an S3 class would be constructor
function, and a print function that overrides the default print method
for a list. (Hadley also recommends a validator function for any S3
class). We implement `print.note()` indirectly, using `validate.note()`
and `format.note()`. That makes it slightly easier to use (e.g.)
`cat()`.

``` r
new_note <- function(name, accidentals, octave, duration) {
  stopifnot(is.character(name) && length(name) == 1)     # Name is a single character
  stopifnot (grepl('[A-G]', name))
  
  stopifnot(is.numeric(accidentals) && accidentals%%1 == 0 && abs(accidentals <= 4))
  stopifnot(is.numeric(octave) && octave%%1 == 0 && abs(octave -4 <= 5))
  
  stopifnot (is.numeric(duration) && duration > 0  && 
               duration <= 64 )  # may not ALWAYS work, since depends on note length
  
  note <- structure(list(name = name,
                         accidentals = accidentals,
                         octave = octave,
                         duration = duration),
                    class = 'note')
}
```

Note that this data model carries accidental values with each note. That
means (so far) it does not follow the convention of musical notation of
implicitly adding sharps and flats for notes found in a well-defined
key.

``` r
validate_note <- function(note) {
  result <-  names(note) == c('name', 'accidentals', 'octave', 'duration')

  result <- result &&
      is.character(note$name) && length(note$name) == 1 &&     # Name is a single character
      grepl('[A-G]', note$name)                          # a letter from a to G, uppercase

  result <- result &&
      is.numeric(note$accidentals) && 
      note$accidentals%%1 == 0 && 
      abs(note$accidentals <= 4)
    
  result <- result && 
      is.numeric(note$octave) && 
      note$octave%%1 == 0 && 
      abs(note$octave - 4 <= 5)  # Restricted range of octaves -- is this adequate?
  
  result <- result && 
  is.numeric(note$duration) && 
    note$duration > 0  && 
    note$duration <= 64
  return(result)
  }

format.note <- function(note) {
  if(validate_note(note)) {
    s_or_f <- sign(note$accidental)
    if (s_or_f < 0) {
      acc_str <- paste(rep('b', abs(note$accidental)), collapse = '')
    } else if (s_or_f > 0) {
      acc_str <- paste(rep('#', abs(note$accidental)), collapse = '')
    } else{
      acc_str <- ''
    }
    return(paste0('Note: ', note$name, acc_str, '{', note$octave, '}', note$duration))
  } else {
    stop('Value is not a valid note object!')
  }
}

print.note <- function(note) {
  print(format(note))
}
```

``` r
char = "B"
accidental = -1
octave = 4
duration  = 2

nt <- new_note(name = toupper(char), accidentals = accidental, 
                          octave = octave, duration = duration)
nt
#> [1] "Note: Bb{4}2"
```

## Specifying the Parser

The process of creating the parser specification has a number of quirks
that are not clearly laid out in the documentation. The most important
is how one accesses the object that is being passed along the parsing
chain.

Each parsing function takes two arguments, `doc`, and `p`. I believe the
choice of the name `p` is arbitrary.

The object `p` is an R6 object, a “YaccProduction” that acts something
like a list, except that positional access is not through traditional R
subsetting using `[` and `[[`, but through `$set()` and $get()\`
methods.

`$get(1)` and `$set(1,value)` get and set, respectively the first item
in the related parsing rule : the single item on the left hand side.
Ordinarily, this is the RESULT of the parsing step, built as some sort
of a function of the items passed in in the RHS of the parsing rule.

`$get(2)` retrieves the second item counting from left to right – the
first item on the RHS, while `$get(3)` gets the third item, which
corresponds to the second item on the RHS of the parsing rule, and so
on.

`$length()` tells you how many items were in the parsing rule. We used
that function a lot in early code, but the manual for ‘ply’, an ancestor
to `rly` in Python, recommends against this approach. Separating
different length rules cleanly into different rules, rather than
combining them with OR, tends to be more efficient.

``` r
SmallParser <- R6::R6Class("Parser",
  public = list(
    tokens = TOKENS,  # defined above!
    literals = LITERALS,
    # Parsing rules
    
    p_note_seq = function(doc = 'note_seq : note note_seq
                                          | spaced_note note_seq', p) {
        p$set(1, append(p$get(3), list(p$get(2)), after = 0))  # probably slow....
    },
    
    p_note_seq_start = function(doc = 'note_seq : note
                                                | spaced_note', p) {
        p$set(1, list(p$get(2)))
    },
    
    # the following could be folded back int onote, but eventually 
    # we will want to handle this as a special case too...
    p_spaced_note = function(doc = 'spaced_note : note WHITESPACE', p) {
      p$set(1, p$get(2))
    },
    
    p_note  = function(doc = 'note : fullpitch', p) {
        p$set(1, p$get(2))
    },
    
     p_note_duration  = function(doc = 'note : fullpitch duration', p) {
        note <- p$get(2)
        note$duration <- p$get(3)
        p$set(1, note)
    },
    
    p_duration = function(doc = 'duration : DURATION', p) {
      p$set(1, strtoi(p$get(2)))
    },
    
    p_fullpitch = function(doc = 'fullpitch : notename', p) {
      p$set(1, p$get(2))   # is this needed?
    },
    
    p_fullpitch_adjust = function(doc = 'fullpitch : notename OCTAVE_ADJ', p) {
        note <- p$get(2)
        chars <- p$get(3)
        chars <- strsplit(chars, '')
        octave <- note$octave
        for (n in seq_along(chars)) {
          if (chars[n] == "'") {
            octave = octave + 1
          } else if (chars[n] == ',') {
            octave = octave - 1
          }
        }
        note$octave <- octave
        p$set(1, note)
    },
    
    p_notename =  function(doc = 'notename : basenote', p) {
        p$set(1, p$get(2))   # is this needed?
    },
    
    
    p_notename_accidental =  function(doc = 'notename : ACCIDENTAL basenote', p) {
      note <- p$get(3)
      chars <- p$get(2)
      chars <- strsplit(chars, '')
      accidental <- note$accidental
      for (n in seq_along(chars)) {
        if (chars[n] == '^') {
          accidental = accidental + 1
        } else if (chars[n] == '_') {
          accidental = accidental - 1
        }
      }
      note$accidentals <- accidental
      p$set(1, note)
    },
    
    # we start with basenote, to create a new note data structure, which we modify
    # as we add other components to determine true_pitch and pitch duration.
    p_basenote    = function(doc = 'basenote : BASENOTE', p) {
      char <- p$get(2)
      if (char == toupper(char)) {
        octave <- 4
      } else {
        octave <- 5
      }
      # create our note data structure
      p$set(1, new_note(name = toupper(char), accidental = 0, 
                        octave = octave, duration = 1))
    },
    
    p_error = function(p) {
      if(is.null(p)) cat("Syntax error at EOF")
      else           cat(sprintf("Syntax error at '%s'", p$value))
    }
  )
)

smallparser <- yacc(SmallParser, debug = TRUE)
#> Generating LALR tables
```

# Testing the Parser

``` r
p <- smallparser$parse(arpeggios_1, smalllexer)
p
#> [[1]]
#> [1] "Note: C{4}1"
#> 
#> [[2]]
#> [1] "Note: E{4}1"
#> 
#> [[3]]
#> [1] "Note: G{4}2"
#> 
#> [[4]]
#> [1] "Note: C{4}1"
#> 
#> [[5]]
#> [1] "Note: F{4}1"
#> 
#> [[6]]
#> [1] "Note: A{4}2"
#> 
#> [[7]]
#> [1] "Note: D{4}1"
#> 
#> [[8]]
#> [1] "Note: G{4}1"
#> 
#> [[9]]
#> [1] "Note: B{4}2"
```

It’s not too hard to assemble that into a dataframe. The only trick is
that we have to unclass the note objects. (Alternatively, we could
define an `as.data.frame.note()` function that does that for us).

``` r
do.call(rbind, lapply(p, function(note) data.frame(unclass(note))))
#>   name accidentals octave duration
#> 1    C           0      4        1
#> 2    E           0      4        1
#> 3    G           0      4        2
#> 4    C           0      4        1
#> 5    F           0      4        1
#> 6    A           0      4        2
#> 7    D           0      4        1
#> 8    G           0      4        1
#> 9    B           0      4        2
```

Or Flatten the list of notes to a vector representation of the strings,
and show it.

``` r
v <- vector(length = length(p))
for (n in seq_along(p)) {
  cat(format(p[[n]]))
  cat('\n')
}
#> Note: C{4}1
#> Note: E{4}1
#> Note: G{4}2
#> Note: C{4}1
#> Note: F{4}1
#> Note: A{4}2
#> Note: D{4}1
#> Note: G{4}1
#> Note: B{4}2
```

``` r
p <- smallparser$parse(arpeggios_2, smalllexer)
do.call(rbind, lapply(p, function(note) data.frame(unclass(note))))
#>   name accidentals octave duration
#> 1    G           0      4        1
#> 2    B           0      4        1
#> 3    D           0      4        2
#> 4    G           0      4        1
#> 5    C           0      4        1
#> 6    E           0      4        2
#> 7    A           0      4        1
#> 8    D           0      4        1
#> 9    F           1      4        2
```

``` r
p <- smallparser$parse(arpeggios_3, smalllexer)
do.call(rbind, lapply(p, function(note) data.frame(unclass(note))))
#>   name accidentals octave duration
#> 1    G           0      4        1
#> 2    B          -1      4        1
#> 3    D           0      4        2
#> 4    G           0      4        1
#> 5    C           0      4        1
#> 6    E          -1      4        2
#> 7    A           0      4        1
#> 8    D           0      4        1
#> 9    F           0      4        2
```

``` r
p <- smallparser$parse(arpeggios_4, smalllexer)
do.call(rbind, lapply(p, function(note) data.frame(unclass(note))))
#>   name accidentals octave duration
#> 1    G           0      4        1
#> 2    B           0      4        1
#> 3    D           0      5        2
#> 4    G           0      4        1
#> 5    C           0      5        1
#> 6    E           0      5        2
#> 7    A           0      4        1
#> 8    D           0      5        1
#> 9    F           1      5        2
```

``` r
p <- smallparser$parse(arpeggios_5, smalllexer)
do.call(rbind, lapply(p, function(note) data.frame(unclass(note))))
#>   name accidentals octave duration
#> 1    G           0      4        1
#> 2    B           0      4        1
#> 3    D           0      5        2
#> 4    G           0      4        1
#> 5    C           0      5        1
#> 6    E           0      5        2
#> 7    A           0      4        1
#> 8    D           0      5        1
#> 9    F           1      5        2
```

``` r
p <- smallparser$parse(arpeggios_6, smalllexer)
do.call(rbind, lapply(p, function(note) data.frame(unclass(note))))
#>   name accidentals octave duration
#> 1    G           0      4        1
#> 2    B           0      4        1
#> 3    D           0      5        2
#> 4    G           0      4        1
#> 5    C           0      5        1
#> 6    E           0      5        2
#> 7    A           0      4        1
#> 8    D           0      5        1
#> 9    F           1      5        2
```

# Other R Parsing Engines

Google Searches turn up the following parsers in R.

Minimalist regex-based parsing engine – not so useful for nested
grammars. Flexo <https://github.com/coolbutuseless/flexo>

rly package <https://cran.r-project.org/web/packages/rly/index.html>
formal impelentation of lex and yacc in R.

dparser package
<https://cran.r-project.org/web/packages/dparser/index.html>

minilexer package
<https://coolbutuseless.bitbucket.io/2018/03/26/parsing-data-part-1.-the-minilexer-package---a-simple-lexer-in-r/>

# R Packages that simplify Tree Data Structires

<https://cran.r-project.org/web/packages/data.tree/vignettes/data.tree.html>

# A Formal Grammear

Henrik Norbeck, in 1997 released a formal BNF grammar for ABC 1.6.
Although it is slightly out of date, and incomplete, it provides useful
insight into what is needed to parse a whole ABC file.
<http://web.archive.org/web/20080309023424/http://www.norbeck.nu/abc/abcbnf.htm>