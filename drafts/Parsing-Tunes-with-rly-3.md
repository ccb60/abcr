Parsing ABC Tunes with `rly`
================
Curtis C. Bohlen
11/14/2021

-   [Introduction](#introduction)
    -   [The Structure of `rly`](#the-structure-of-rly)
    -   [R6 Classes and `rly`](#r6-classes-and-rly)
-   [Load libraries](#load-libraries)
-   [A `note` Class](#a-note-class)
    -   [A Constructor Function](#a-constructor-function)
    -   [A Validator Function](#a-validator-function)
    -   [A `format()` Function](#a-format-function)
    -   [A `print()` Function](#a-print-function)
    -   [An `as.data.frame()` function](#an-asdataframe-function)
-   [A Simple Note Lexer and Parser](#a-simple-note-lexer-and-parser)
    -   [The Lexer](#the-lexer)
        -   [Demos](#demos)
        -   [What’s in a Lexer?](#whats-in-a-lexer)
    -   [The Format of the Parser](#the-format-of-the-parser)
        -   [Specifying the Parser](#specifying-the-parser)
        -   [Tiny Parser](#tiny-parser)
        -   [Testing](#testing)
-   [A Slightly More Complex Example](#a-slightly-more-complex-example)
-   [Test Data](#test-data)
-   [A Simple Tune Lexer](#a-simple-tune-lexer)
-   [A Simple Tune Parser](#a-simple-tune-parser)
-   [Testing the Parser](#testing-the-parser)

# Introduction

In this notebook, I explore the R parsing engine `rly`. `rly` is a port
to R of a python package, `ply`. The name `ply` implies “python lex and
yacc”, where “lex” and “yacc” are classic UNIX tools for building formal
parsers.

Unfortunately, the documentation for `rly` is sparse, and there are few
examples available on-line, so it takes some time to understand how to
work with the package. One can get some idea of how everything works by
digging into the package’s precursors, especially `ply`, which is better
documented, but details of usage in R generally need to be figured out
via introspection, inspection of source code, or trial and error.

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

``` r
#library(tidyverse)
#library(dplyr)
#library(tibble)
#library(abcr)
#library(stringr)
library(rly)
#library(data.tree)
```

# A `note` Class

In ABC, the concept of a NOTE consists of the combination of accidentals
(sharps and flats), a note name, an octave specification, and a
duration, in that order.

We want to encapsulate that idea in a data structure, to simplify
working with notes and tunes.

We could construct an R6 class for this purpose, a reasonable choice
since we are working with R6 classes with the `rly` package already. But
R6 is not very much in keeping with R idioms, and we would like this
data structure to have other uses.

The alternative is to ‘build’ an S3 class. But S3 “class” is all about
method dispatch, so what we would need clarity on what methods would
simplify coding the parser.

The main thing we would want for an S3 class would be constructor
function, and a print function that overrides the default print method
for a list. (Hadley also recommends a validator function for any S3
class). We implement `print.note()` indirectly, using `validate.note()`
and `format.note()`. That makes it slightly easier to use (e.g.)
`cat()`.

It is also convenient to create a `as.data.frame.note()` function, to
simplify combining a sequence of notes into a dataframe.

## A Constructor Function

``` r
new_note <- function(name, accidentals, octave, duration) {
  stopifnot(is.character(name) && length(name) == 1)  # Name is a single char
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

## A Validator Function

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
```

## A `format()` Function

``` r
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
    stop('Not a valid note object!')
  }
}
```

## A `print()` Function

``` r
print.note <- function(note) {
  print(format(note))
}
```

## An `as.data.frame()` function

t’s not too hard to assemble that into a dataframe. The only trick is
that we have to unclass the note objects. (Alternatively, we could
define an `as.data.frame.note()` function that does that for us).

``` r
as.data.frame.note <- function(note) {
  if (validate_note(note)) {
    data.frame(unclass(note))
  } else
    stop('Not a valid note object!')
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

``` r
as.data.frame(nt)
#>   name accidentals octave duration
#> 1    B          -1      4        2
```

Now, if you monkey with the contents of a note object, so that it is no
longer a valid note, you get errors….

``` r
nt$accidentals='sharp'
validate_note(nt)
#> [1] FALSE
nt
#> Error in format.note(note): Not a valid note object!
```

``` r
as.data.frame(nt)
#> Error in as.data.frame.note(nt): Not a valid note object!
```

# A Simple Note Lexer and Parser

The full ABC specification is considerably more complex, but a major
purpose of this Notebook is to share lessons learned while approaching
that complex problem. So here, I’ll build a toy lexer and parser that
accepts an ABC note specification, and converts it to a `note` object.

## The Lexer

A Lexer accepts a stream of text input, and identifies discrete tokens.
Each token is a molecule of meaning that you may want to interpret
further as you parse the text.

Each token is defined either as a literal, via a REGEX, or via a
function. The choice of what to code each way has implications for
precedence, but for now, we only use REGEX expressions.

ABC notes are basically letters indicating pitch names from A to G. To
code the two octaves beginning with middle C, which correspond to melody
lines in a lot of folk music, Those letters can be capitalized (Octave
4) or not (octave 5). Those bare pitch names can be decorated with other
information, especially with one or more leading sharp or flat
indicators, one or more trailing octave adjustment indicators, and a
following number that indicates the note duration (usually in eighths).

In constructing the R6 object that will produce the parser, we define a
list of tokens, a list of literals, and specially-named REGEX strings or
functions, each of which has a name that begin with a “t-”. Long REGEXES
are matched before shorter ones. The order of functions matters. Strings
caught in earlier functions don’t get caught by later ones. Only items
that “fall through” all functions and REGEXes hit `t-error()`.

``` r
TOKENS = c('ACCIDENTALS', 'BASENOTE', 'OCTAVE_ADJ', 'DURATION')
LITERALS = c()

TinyLexer <- R6::R6Class("Lexer",
  public = list(
    tokens = TOKENS,
    literals = LITERALS,
    
    # The ABC standard allows multiple accidental markers, possibly mixed
    t_ACCIDENTALS = '[\\^=_]+',
    t_BASENOTE = "[a-gA-G]",
    t_OCTAVE_ADJ = "[\\',]+",
    
    t_DURATION = function(re='[\\/]?\\d+', t) {
         first <- substr(t$value, 1, 1)
         if (first == '/') {
           rest <- sub('.', '', t$value)  # strip first character
           t$value <- 1 / strtoi(rest)    # Convert to integer and invert
         } else {
           t$value <- strtoi(t$value)
         }
         return(t)
    },
    
    # we Need a Function to Increment Line Numbers, to help with fixing errors
    t_newline = function(re='\\n+|(?:\\n\\r)+', t) {         
      t$lexer$lineno <- t$lexer$lineno + nchar(t$value)
      return(NULL)
    },
    
    # define an error function for unrecognized tokens or characters,
    # here we simply skip to the next character and try again.
    t_error = function(t) {
      cat(sprintf("..........Illegal character '%s'\n", t$value[1]))
      t$lexer$skip(1)
      return(t)
    
    }
  )
)

tinylexer <- rly::lex(TinyLexer)
```

### Demos

#### Convenience Function

``` r
strip_lexer <- function(thelexer) {
  while (!is.null(current_token <- thelexer$token())) {
   print(current_token)
  }
}
```

``` r
test_notes <- c('B','B/2', '^G,4', '^^G,4', '^Q,4')
for (note in test_notes) {
    tinylexer$input(note)
    strip_lexer(tinylexer)
    cat('\n')
}
#> LexToken(BASENOTE,B,1,1)
#> 
#> LexToken(BASENOTE,B,1,1)
#> LexToken(DURATION,0.5,1,2)
#> 
#> LexToken(ACCIDENTALS,^,1,1)
#> LexToken(BASENOTE,G,1,2)
#> LexToken(OCTAVE_ADJ,,,1,3)
#> LexToken(DURATION,4,1,4)
#> 
#> LexToken(ACCIDENTALS,^^,1,1)
#> LexToken(BASENOTE,G,1,3)
#> LexToken(OCTAVE_ADJ,,,1,4)
#> LexToken(DURATION,4,1,5)
#> 
#> LexToken(ACCIDENTALS,^,1,1)
#> ..........Illegal character 'Q'
#> LexToken(error,Q,1,2)
#> LexToken(OCTAVE_ADJ,,,1,3)
#> LexToken(DURATION,4,1,4)
```

### What’s in a Lexer?

We use a little introspection to understand the structure of a Lexer,
created from a Lexer Generator, with the function `lex()`.

#### Methods

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

#### Members

The Man pages suggest only a couple of the attributes of a Lexer are
intended for public use:

lineno: Current line number

lexpos: Current position in the input string

Numerous other attributes are used by the lexer internally to manage the
lexing process.

#### LexTokens

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
tinylexer$input("A,3")
(tok <- tinylexer$token())
#> LexToken(BASENOTE,A,1,1)
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
#> LexToken(SLOPPY!,A,7,17)
```

Two members of the class that LOOK useful are often NA, presumably,
these are only instantiated during actual lexing or parsing. Not sure
how that magic is accomplished.

``` r
tok$lexer
#> [1] NA
tok$parser
#> [1] NA
```

## The Format of the Parser

The general steps are similar to how we produced a lexer: we have to
define the structure of a parser with a combination of tokens, literals,
and parsing functions.

The rules of the parser here take on a form that was inherited from the
original `yacc` program, and redefined in python’s `ply`. Both are based
on various ideas about formal languages, grammars and parsers that are
well beyond the scope of what I’m doing here.

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

The primary responsibility of each function is to transform the values
of P, which are then passed on to the next step in the parser, and so
on. If the input string is successfully parsed, the parser returns the
final value of p, usually some sort of useful data structure.

Each function can, in principal, also alter external data structures,
set flags, etc. That may be useful to alter behavior of the parser,
store variable names, etc.

### Specifying the Parser

The process of creating the parser specification has a number of quirks
that are not clearly laid out in the documentation. The most important
is how one accesses the object that is being passed along the parsing
chain.

Each parsing function takes two arguments, `doc`, and `p`. I believe the
choice of the name `p` is arbitrary.

The function parameter, `p`, is an R6 object, a “YaccProduction”.
Documentation on the nature of this object is sparse. The class is a
wrapper around the objects actually passed to each grammar rule. In
practice it acts something like a list, except that positional access is
not through traditional R subsetting using `[` and `[[`, but through
`$set()` and `$get()` methods.

`$get(1)` and `$set(1,value)` get and set, respectively the first item
in the related parsing rule : the single item on the left hand side
(LHS). Ordinarily, this is the RESULT of the parsing step, built as some
sort of a function of the items passed in in the RHS of the parsing
rule. Thus most parsing rules wil lend with a call to `p$set(1,value)`.

`$get(2)` retrieves the second item counting from left to right – the
first item on the RHS, while `$get(3)` gets the third item, which
corresponds to the second item on the RHS of the parsing rule, and so
on.

`$length()` tells you how many items are in the parsing rule. We used
that function a lot in early code, but the manual for ‘ply’, an ancestor
to `rly` in Python, recommends against that approach. Separating
different length rules cleanly into different rules, rather than
combining them with OR, tends to be more efficient. We have refactored
the following parsers to avoid reliance on `p$length()`.

### Tiny Parser

``` r
TinyParser <- R6::R6Class("Parser",
  public = list(
    tokens = TOKENS,  # defined above!
    literals = LITERALS,
    # Parsing rules
    
    
    # The first rule is the largest structure you want to parse
    # Here, that is a note
    # In a formal language, this is the START condition
    p_note  = function(doc = 'note : note_dur
                                   | note_oct
                                   | note_acc
                                   | basenote', p) {
        p$set(1, p$get(2))
    },
    
    # If the note has a duration specified, we need to add that now.
    # we could be working with three possible precursors
     p_note_duration  = function(doc = 'note_dur : note_oct DURATION
                                                 | note_acc DURATION
                                                 | basenote DURATION', p) {
        note <- p$get(2)
        note$duration <- p$get(3)
        p$set(1, note)
    },
    
    
    # If we have octave adjustments, they are signaled after the basenote
    # but now we need to recognize that we could be working with a bare
    # basenote, or one modified by accidentals
    p_note_oct = function(doc = 'note_oct : note_acc OCTAVE_ADJ
                                          | basenote OCTAVE_ADJ', p) {
        note <- p$get(2)
        chars <- p$get(3)
        chars <- strsplit(chars, '')[[1]]  # strsplit is vectorized
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
    
    
    # If we have one or more accidentals, they come before the base note
    # the ABC standard allows multiple accidentals
    p_note_acc=  function(doc = 'note_acc : ACCIDENTALS basenote', p) {
      note <- p$get(3)
      chars <- p$get(2)
      chars <- strsplit(chars, '')[[1]]  # strsplit is vectorized
      accidental <- note$accidentals

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
      if(is.null(p)) cat("**********Syntax error at EOF\n")
      else           cat(sprintf("**********Syntax error at '%s'\n", p$value))
    }
  )
)

tinyparser <- yacc(TinyParser, debug = TRUE)
#> Generating LALR tables
```

### Testing

``` r
test_notes <- c('B','B/2', '^G,4', '^^G,4', '^Q,4')
for (note in test_notes) {
    print(tinyparser$parse(note, tinylexer))
    cat('\n')
}
#> [1] "Note: B{4}1"
#> 
#> [1] "Note: B{4}0.5"
#> 
#> [1] "Note: G#{3}4"
#> 
#> [1] "Note: G##{3}4"
#> 
#> ..........Illegal character 'Q'
#> **********Syntax error at 'Q'
#> NULL
```

# A Slightly More Complex Example

This lexer and parser accept a string of notes, with white space and
other complexities, so it is one step more complex than the one we just
developed.

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

``` r
TOKENS = c('ACCIDENTALS', 'BASENOTE', 'OCTAVE_ADJ', 'DURATION', 'WHITESPACE')
LITERALS = c()

SmallLexer <- R6::R6Class("Lexer",
  public = list(
    tokens = TOKENS,
    literals = LITERALS,
  
    # The ABC standard allows multiple accidental markers, possibly mixed
    t_ACCIDENTALS = '[\\^=_]+',
    t_BASENOTE = "[a-gA-G]",
    t_OCTAVE_ADJ = "[\\',]+",
    
    t_DURATION = function(re='[\\/]?\\d+', t) {
         first <- substr(t$value, 1, 1)
         if (first == '/') {
           rest <- sub('.', '', t$value)  # strip first character
           t$value <- 1 / strtoi(rest)    # Convert to integer and invert
         } else {
           t$value <- strtoi(t$value)
         }
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

# A Simple Tune Parser

Next, we build a simple parser to take the output from a lexer and turn
in into something more useful. Our goal here is some sort of data
structure that can be converted into useful forms, such as a data frame
of notes or a musicXML file. Here we focus on generating a list of
notes, where a note contains complete pitch and duration information.

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
    
    p_note  = function(doc = 'note : note_dur
                                   | note_oct
                                   | note_acc
                                   | basenote', p) {
        p$set(1, p$get(2))
    },
    
    # If the note has a duration specified, we need to add that now.
    # we could be working with three possible precursors
     p_note_duration  = function(doc = 'note_dur : note_oct DURATION
                                                 | note_acc DURATION
                                                 | basenote DURATION', p) {
        note <- p$get(2)
        note$duration <- p$get(3)
        p$set(1, note)
    },
    
    
    # If we have octave adjustments, they are signaled after the basenote
    # but now we need to recognize that we could be working with a bare
    # basenote, or one modified by accidentals
    p_note_oct = function(doc = 'note_oct : note_acc OCTAVE_ADJ
                                          | basenote OCTAVE_ADJ', p) {
        note <- p$get(2)
        chars <- p$get(3)
        chars <- strsplit(chars, '')[[1]]  # strsplit is vectorized
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
    
    
    # If we have one or more accidentals, they come before the base note
    # the ABC standard allows multiple accidentals
    p_note_acc=  function(doc = 'note_acc : ACCIDENTALS basenote', p) {
      note <- p$get(3)
      chars <- p$get(2)
      chars <- strsplit(chars, '')[[1]]  # strsplit is vectorized
      accidental <- note$accidentals

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
for (n in seq_along(p)) {
  cat(format(p[[n]]))
  cat('\n')
}
#> Note: G{4}1
#> Note: B{4}1
#> Note: D{4}2
#> Note: G{4}1
#> Note: C{4}1
#> Note: E{4}2
#> Note: A{4}1
#> Note: D{4}1
#> Note: F#{4}2
```

``` r
p <- smallparser$parse(arpeggios_3, smalllexer)
for (n in seq_along(p)) {
  cat(format(p[[n]]))
  cat('\n')
}
#> Note: G{4}1
#> Note: Bb{4}1
#> Note: D{4}2
#> Note: G{4}1
#> Note: C{4}1
#> Note: Eb{4}2
#> Note: A{4}1
#> Note: D{4}1
#> Note: F{4}2
```

``` r
p <- smallparser$parse(arpeggios_4, smalllexer)
for (n in seq_along(p)) {
  cat(format(p[[n]]))
  cat('\n')
}
#> Note: G{4}1
#> Note: B{4}1
#> Note: D{5}2
#> Note: G{4}1
#> Note: C{5}1
#> Note: E{5}2
#> Note: A{4}1
#> Note: D{5}1
#> Note: F#{5}2
```

``` r
p <- smallparser$parse(arpeggios_5, smalllexer)
for (n in seq_along(p)) {
  cat(format(p[[n]]))
  cat('\n')
}
#> Note: G{4}1
#> Note: B{4}1
#> Note: D{5}2
#> Note: G{4}1
#> Note: C{5}1
#> Note: E{5}2
#> Note: A{4}1
#> Note: D{5}1
#> Note: F#{5}2
```

``` r
p <- smallparser$parse(arpeggios_6, smalllexer)
for (n in seq_along(p)) {
  cat(format(p[[n]]))
  cat('\n')
}
#> Note: G{4}1
#> Note: B{4}1
#> Note: D{5}2
#> Note: G{4}1
#> Note: C{5}1
#> Note: E{5}2
#> Note: A{4}1
#> Note: D{5}1
#> Note: F#{5}2
```
