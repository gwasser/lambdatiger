Using Happy for simple parsing in Haskell
=========================================

I previously discussed how to implement a simple lexer for the Tiger language in
Haskell by using the lexer generator alex. In this post, I want to move on to
Chapters 3 and 4 of Appel's book to implement the parser for the Tiger language.

# Parsing Theory

Parsing, also known as *syntactic analysis*, is basically checking
that a sequence of tokens forms a grammatically correct sentence within a given
language. We're dealing with computer programming languages here (particularly,
the Tiger language), but the concept applies equally well to human languages. In
English, parsing would be the idea of checking that a string of words is
grammatically correct. "The rainbow ate staplers" is *syntactically* correct
because it follows the standard subject-verb-object form of a noun-verb-noun,
but it is *semantically* meaningless because rainbows cannot eat and even if they
could would probably not eat staplers.

I rushed over it a bit last post but the so-called "front-end" of the compiler
is the part that deals with transforming the input into an intermediate
representation (IR) that is designed to be useful for optimization or code
analysis tasks such as finding vulnerabilities before the final result is output
from the compiler. The front end is often broken into three stages:

* **Lexical** analysis, to read the raw input and turn it into a canonical token
  stream.
* **Syntactic** analysis (or parsing), to check that the token string is
  valid for the given programming language grammar, and turn it into a syntax
  tree representing the input.
* **Semantic** analysis, to convert the syntax tree into an intermediate
  representation after checking that the input "makes sense" in terms of its
  meaning (for example, do type-checking to ensure we're adding two numbers,
  and not doing something meaningless like adding an integer to a string).

In a future post, I'll write about semantic analysis and type-checking, but the
rest of this post is about the syntactic analysis phase for Tiger. There's a lot
of material here; Chapters 3 and 4 of Appel, but also all of Chapters 4 and 5 in
Aho which is over 150 pages (most of it albeit different algorithm
implementations). So I will set some basic definitions for describing
programming languages and their grammars, and refer interested readers to the
books so that I can focus on implementation.

A *language* is a set of strings that are made up of input symbols (the
*alphabet*) that are allowed to be used in writing "words" (*tokens*). The
alphabet could be the English alphabet, or the Cyrillic alphabet, or the ASCII
character set for computer languages; the alphabet of only 1 and 0 works
perfectly fine for a binary language. The strings of a language are basically
the sequences of tokens that are accepted by the language's *grammar*, a set
of rules that allow us to confirm whether or not a string is indeed part of a
given language.

Linguists study what types of properties different grammars have, and so have
classified languages based on these properties. Sometimes this classification is
referred to as the [Chomsky hierarchy][1]. A full discussion is a bit beyond scope
for me, but the short version is that regular expressions and NFAs/DFAs are
incapable of representing certain language constructs important to programming
languages, such as checking that every open parenthesis has a closing
parenthesis, or describing nested data structures. Therefore, most programming
languages end up requiring parsers based on *context-free languages* (CFLs), which
are more powerful than regular languages and do support nested data structures.
In fact, many languages require some features of *context-sensitive languages*
(CSLs), but in practice we can usually get by using CFLs and tweaking the
parsing algorithm to handle context-sensitive corner cases.

## Context-free grammars

CFL grammars are usually described in Backus-Naur Form (BNF). Grammars
are made up of a series of rules called *productions* that describe how one can
create sentences in the language from scratch. Productions are usually written
in the form of `symbol -> sym1 sym2 ... symN`, which would mean that if you see
`symbol` in a sentence in the language, you can replace `symbol` with the sequence
`sym1 sym2 ... symN` and get a new sentence in the language. The symbols in
productions can be one of two types: *non-terminal*, meaning a production rule
exists that allows you to replace the symbol with another sequence of symbols
(in other words, non-terminal symbols are ones that appear on the left side of
an arrow in a production), or *terminal*, meaning the symbol is part of the
language and cannot be substituted. Terminals are typically the keywords of a
language, or literals such as numbers and strings, whereas non-terminals
represent the "parts of speech" of the grammar (such as *noun*) and are
placeholders for sequences of terminals. A particular non-terminal is chosen as
the *start symbol*, which defines the high-level grammar; all sentences begin by
first applying the production rule associated with the start symbol, then using
other productions as needed to build the sentence desired.

Notice in our general BNF production `symbol -> sym1 sym2 ... symN`, we replace
only one symbol (on the left side of the arrow) by a sequence of one or more
symbols (on the right side of the arrow). In other words, there's no
restrictions; any time we see symbol, we can replace it with the right hand side
of the arrow (`sym1 sym2 ... symN`). The fact that we can replace the symbol
anytime we see it is why the grammar is called *context-free*.
A *context-sensitive* grammar can only perform this substitution if the symbol
on the left side is seen in a particular context (that is, with certain other
symbols around it; you can't just replace it whenever you want, but only if it
is in the right context!). The context-sensitive rules allow us to build
stronger but slower, more complex parsers, while context-free grammars have
simpler, more efficient algorithms since we don't have to ever worry about the
context of surrounding tokens. Therefore, we in general try to get away with
context-sensitive rules and algorithms for as long as we can to maintain
simplicity and performance.

How are CFL parsers implemented? It depends on the algorithm used; recursive
descent parsers use a technique known as *top-down parsing*, whereas most parser
generators use *bottom-up parsing*. Bottom-up parsers essentially read the
production rules backwards; the parser reads in one token at a time, and checks
if a production rule exists that has the stream of tokens already seen on the
right side of the arrow. If so, then it labels the token sequence with the
non-terminal on the left side of the production rule, and moves on to the next
token(s). It keeps doing this until it processes all input. If we can use
production rules on the input to reach the starting state of the grammar, we
consider the input successfully parsed; otherwise, we return an error.

The most common bottom-up parsing algorithm is known as LR(k), with k
representing the amount of "lookahead" of the parser. Lookahead is used to
determine which production rule to use by checking which token(s) come(s) next
in the input, and comparing the next token(s) with available production rules.
Most common perhaps is LR(1) parsing, which uses only a single lookahead token,
meaning the parser checks only the very next token in the input before deciding
which production rule to use on the currently processed tokens; LR(0) doesn't
check at all for what token comes next, and so just guesses on-the-fly which
production rule to use. As you can imagine, LR(0) is simpler since it doesn't
need to track lookaheads, but is also not as powerful as LR(1) for the same
reason and so cannot be used to parse many languages including real-world
programming languages. Due to large memory requirements of implementing a full
LR(1) parser (keeping track of lookahead and how that affects the states of the
parser), many parser generators actually generate LALR(1) parsers, which are
sort of a hybrid between LR(0) and LR(1). LALR(1) tries to control memory usage
by behaving a lot like LR(0), but using a lookahead when necessary for states
that require it. Technically speaking, LALR is not as powerful as full LR, but
in practice it is usually close enough.

## The parse tree

Since every language has a designated start symbol, we can view a successful
parse as building out syntactic structure from a root "node" with each
production rule is viewed as a child node from the root node. With this view,
the syntactic structure is a *tree*.

Therefore, the parser is effectively translating a sequence of input tokens into
a tree data structure that represents the syntax of the input. Tree structures
are very important to computer science, and a lot of algorithms on trees are
known.

Depending on the order that production rules are applied, we can get different
parse trees. In some cases, the grammar may have ambiguity; that is, more than
one production rule can be applied to a given input token stream, meaning the
input might be represented by more than one parse tree. Ideally, we'd design our
language grammar in a way that prevents ambiguity and production rule conflicts,
but for many reasons this isn't always desired or possible.

Since different parse trees may have different meanings (semantics), we must set
rules for how to disambiguate the grammar if we cannot make it unambiguous. Two
new rules handle most scenarios: first, we go with the *longest match* rule that
says if given two possible productions, we choose the one that lets us keeping
matching (contains non-terminals) over the one that leads to a shorter terminal
sentence. Second, we set *precedence rules*, so that if given two possible
productions that both lead to non-terminals, we let the grammar writer decide
which one is the default by setting a precedence for each of the conflicting
rules. The longest match rule addresses *shift-reduce* conflicts (so-called
because the conflict is between shifting another token onto the stack to look
for a longer match, or stopping to apply a production and reduce before moving
on to the next token), while precedence rules address *reduce-reduce* conflicts.

# `happy` for parsing

While most sources claim recursive descent parsing algorithms
are easy enough to implement by hand, they also state the task is very tedious
and so make the recommendation of using a *parser generator*. A parser generator
takes in a grammar file (a text file with grammar production rules written in a
format similar to BNF) and generates a parser implementation for that grammar in
the programming language of your choice. Aside from simplifying the work a bit
but generating DFAs for you, even if a recursive descent parser is a good
option, parser generators typically use algorithms that are faster than
recursive descent and are good to at least see if they meet your needs.

The standard parser generator for the C-language is `yacc` (or the GNU
implementation `bison`, which gets a chuckle from me for creative naming), but
most common languages have an equivalent. The Haskell equivalent is called `happy`
(`happy` even uses a similar grammar file format to `yacc`). Therefore, I will for
now use `happy` to generate a parser from a grammar file, and perhaps return to
the idea of writing my own parser from scratch for knowledge purposes at a later
date.

The [`happy` User Guide][2] has some good information and starter examples, but like
many websites and books, uses the example of simple arithmetic. I don't know
what the aversion is to providing parsing tutorials for more legitimate
programming languages or other tasks instead of toy calculators. So, I will walk
through my parser grammar file and point out some things that tripped me up.
It's useful to know before we get started that `happy` by default generates a
LALR(1) parser; understanding what it is doing "under the hood" is important
when writing a grammar so we know how production rule conflicts come about and
how to resolve them.

The first section of the `happy` grammar file `Parser.y` is to define the `Parser`
module, and import the lexer `Tokens` we expect to see as input. Additionally, we
have defined an AST module that we'll talk about in a minute.

    {
    module Tiger.Syntax.Parser (happyTokenParse) where
    import Tiger.Lexical.Tokens (Token(..))
    import Tiger.Syntax.AST
    }

In the header part of the actual grammar file, we have a few special keywords:

    %name happyTokenParse
    %tokentype { Token }
    %error { parseError }

The first directive `%name` sets the name of the output parser function from
`happy`. By default, the function is named `happyParse` if you omit the `%name`
directive. The `%tokentype` directive defines what type of object to use as your
lexer token input; in our case, we defined our lexical tokens in a type named
`Token`. Finally, the `%error` directive allows you to state what function or action
will be taken whenever the parser encounters an error. Here, we tell `happy` to
call the `parseError` function anytime an error is detected; we'll define
`parseError` later.

Next, we define the precedence of operator symbols that essentially defines the
order of operations and allows us to deal with ambiguity in the grammar:

    %right IN OF ELSE
    %left '|'
    %left '&'
    %nonassoc '>' '<' '>=' '<=' '=' '<>'
    %left '+' '-'
    %left '*' '/'
    %left NEG

As you might expect, `%left` means a left-associative operation, and `%right` means
a right-associative option. `%nonassoc` means the operation doesn't associate at
all, meaning you can't write sequences of two or more operators without using
parentheses to specify the intended grouping.

The order these rules are written in is important too. It's a bit
counter-intuitive to me, but the rules are written in order from lowest
precedence to highest precedence (`NEG` at the bottom stands for negation, which
is higher precedence than multiplication or division, which is in turn higher
precedence than addition and subtraction, and so on).

You might be surprised to see some keywords like `IN` and `ELSE` listed as
right-associative operators. This is basically a way of setting the longest
match rule and dealing with shift-reduce conflicts: setting these keywords as
right-associate essentially guarantees that the parser will keep trying to match
rules on the right of the keyword first, leading to longer matches. Likewise,
setting the precedence is a way of dealing with reduce-reduce conflicts, with is
particular important for arithmetic. For example, how do you parse 1+2*3?
There's two possibilities: do the 1+2 first, then multiply by three, leading to
a parse tree of (1+2)*3, or start with the right side and get a parse tree of
1+(2*3). From mathematics, we know the order of operations says that
multiplication must come before addition, and we put this in our parser by
setting multiplication as a higher precedence than addition. Therefore, when the
parse is trying to decide whether to match 1+2 or 2*3 first, it will choose 2*3
because multiplication has the higher precedence than addition.

Next, in our `happy` grammar, we define the tokens expected to be read in by the
parser, in a format such as:

    %token
        ARRAY           { ARRAY }
        ...
        NUM             { NUM $$ }

Each line consists of first, the name of token as used in the grammar production
rules to be defined later in the file, followed by the name of the token from
the input stream in braces. In this case I have chosen to make the names the
same (both are capital `ARRAY`), but they can be different as they are two
distinct types. We repeat this for every type of `Token` input. The entry for `NUM`
is a special case though; our input token was a `NUM` that was a wrapper around an
`Int` that stored the real value of the token internally. The `$$` here means that
the `NUM` on the left of the rule, that is used in grammar production rules later,
actually represents that integer value of the `NUM` rather than the whole `NUM`
`Token`.

## Writing a grammar for Tiger

Designing grammar production rules for the
Tiger language that minimize the numbers of shift-reduce and reduce-reduce
conflicts takes some thought. Many books and online tutorials focus on the
example of arithmetic, and show that one way to resolve ambiguity in the
operators to break up arithmetic expressions into terms and factors, so that we
may use the longest match rule to automatically keep applying production rules
to get factors first before they are added to terms. We don't need to worry
about that trick here since we defined precedence using the special `%left` and
`%right` directives.

Much as `alex` did, `happy` allows you to run functions or actions on the parse
result before returning it, so as with `alex`, we must define a type that
represents the output parse tree nodes that result from each production.

At first, I considered simply making a type that represented each of the grammar
production rules. For instance, since if-then is a different type of statement
in Tiger than if-then-else, my `TreeNode` type contains both an `If` and `IfElse` node
type. It turns out that doing this results in a very cluttered parse tree, since
it is still representing artifacts of the parse process (such as whether or not
there was an else token in the input stream) within the parse tree. This type of
tree is called a *concrete syntax tree* (CST), since it directly represents the
concrete syntax that was parsed. However, this form is cluttered and a bit bulk
to analyze; any code I write to analyze the tree would need to be aware of both
`If` and `IfElse` and take appropriate actions. It's a lot more tedious to write and
maintain.

We can instead make use of `happy`'s ability to apply functions or actions to the
tree nodes before returning, which allows us to do things like combine the `If`
and `IfElse` CST types into a single If type that can represent both (if an else
token was present, record the else expression, but if not, just put a `Nothing` in
that spot). By thinking about this a bit, we can arrive at a much simpler syntax
that is easier to analyze and work with while still accurate portraying the
input syntax. This simpler form of the parse tree is known as an
*abstract syntax tree* (AST). The abstract syntax is what is used in the `AST.hs`
module that was imported into `happy`, which we kept as separate module instead of
defining within the `happy` grammar file for the same reasons we kept the `Tokens`
module separate from `alex`. Appel actually defines a recommended AST for Tiger
in Chapter 4, Figure 4.8, so I went ahead and translated his ML into a Haskell type
definition, and tossed out my old CST type.

A straightforward translation of the grammar rules for Tiger as given in
Appendix A of Appel produced roughly the following grammar:

    program     : exp                                { Program $1 }

    exp         : NIL                                { NilExp }
                | lvalue                             { VarExp $1 }
                | NUM                                { IntExp $1 }
                | STR                                { StrExp $1 }
                | '-' exp %prec NEG                  { OpExp (IntExp 0) Sub $2 }
                | exp '*' exp                        { OpExp $1 Mul $3 }
                | exp '/' exp                        { OpExp $1 Div $3 }
                | exp '+' exp                        { OpExp $1 Add $3 }
                | exp '-' exp                        { OpExp $1 Sub $3 }
                | exp '=' exp                        { OpExp $1 Equal $3 }
                | exp '<>' exp                       { OpExp $1 NotEqual $3 }
                | exp '>' exp                        { OpExp $1 GreaterThan $3 }
                | exp '<' exp                        { OpExp $1 LessThan $3 }
                | exp '>=' exp                       { OpExp $1 GreaterEqual $3 }
                | exp '<=' exp                       { OpExp $1 LessEqual $3 }
                | exp '&' exp                        { IfExp $1 $3 (Just (IntExp 0)) }
                | exp '|' exp                        { IfExp $1 (IntExp 1) (Just $3) }
                | IF exp THEN exp ELSE exp           { IfExp $2 $4 (Just $6) }
                | IF exp THEN exp                    { IfExp $2 $4 Nothing }
                ...

In other words, a program is simply an expression, which may be one of many
types, including various arithmetic expressions, string literals, or
if-then-else statements.

Note that the book suggests to implement the bitwise operators as if-then
expressions to save space in the AST description. We've also implemented
negation as a subtraction operation (also at the book's suggestion), which is
sufficient for problems where we don't expect to overflow memory but isn't
really an industrial-strength implementation. The $1 marks are place holders for
the parsed token type in the grammar rule; each token in the rule is numbered
starting from 1. For example, in the expression `exp '+' exp`, the first token is
exp and labeled `$1`, the second token is '+' and labeled `$2`, and the third token
is again an expression and is labeled `$3`. This allows us to reference them in
our rules, without knowing what they will parse to ahead of time. In the case of
our addition rule example, the rule is `OpExp $1 Add $3` which means to represent
the matched tokens as a single node in the AST, that takes three child nodes:
the first expression, the addition sign (represented by its own token `Add` in the
AST), and the second expression. To save space, I also didn't list every single
rule here, but you can take a look at the implementation on github for the full
list.

However, I quickly found that my straightforward translation produced a `happy`
parser that listed over 300 shift/reduce and 300 reduce/reduce conflicts! To see
what those conflicts were, I had to run `happy` on it's own with the `--info`
flag to get a report on the states and conflicts that `happy` recognizes:

    happy --info Parser.y

Looking at this info seemed a bit overwhelming at first, but if you realize it
is just numbering all of the possible tokens and states being fed into/from the
lexer's DFA, it isn't so bad, just long.

The first error that really popped out at me was multiple conflicts for
expressions (exp). Several rules require (possibly empty) lists or sequences of
expressions to be processed, e.g., function arguments. They look like the
following (naive implementation):

    arglist     : exp ',' arglist            { $1 : $3 }
                | exp                        { [$1] }

I was then using separate rules in the main expression set of rules (shown
above) to cover the case of an empty list. Because the default parser is
bottom-up, it works its way from the bottom of the tree to the top, so when an
expression is found on a leaf node, the parser gets confused which of the many
rules might apply to a lone expression; in other words, is this *just* an
expression, or the end of a list of expressions (a reduce/reduce conflict)? The
parser isn't powerful enough to tell context like that on its own, so we need to
be careful how we write our grammar.

## Parsing sequences

To resolve the issue, I looked at Aho's Chapter 4.3 on how to write a grammar
for a little more guidance on clearing up these conflicts. Unfortunately, it
only covers three possibilities, two of which only apply to top-down parsers
(despite most of the chapter being about bottom-up parsers, and parser
generators which typically use bottom-up parsing themselves). The remaining
method is a way of rewriting if-then-else statements, which is useful but not
very clear about what the general principle is to apply to other ambiguous
productions. As is typical with academic books, I'm left to experiment on my
own.

However, it turns out the `happy` manual has a short section on recommendations
when needing to [parse sequences][3]. The more proper way to write the rules,
to make them left recursive (which produces more efficient parsers via `happy`)
and remove conflicts, is the following:

    arglist     : {- empty -}                 { [] }
                | arglist ',' exp             { $3 : $1 }
                | exp                         { [$1] }

Changing the rules for sequences as suggested cleared away all reduce/reduce
conflicts and got the shift/reduce conflicts down to about 30, a much more
reasonable number. Those conflicts mainly result from arithmetic operator
precedence, so can be safely ignored when the precedence directives are set.

Reading Appel's AST and Appendix A that defines Tiger's production rules
reminded me how important good documentation is. While the AST was helpful, that
isn't until Chapter 4, leaving me a bit lost when initially trying to complete
Chapter 3 (hence why I started down the path of making a CST). The Appendix
definition of the Tiger language made some production rules clear (by writing
them directly in BNF!) but then other rules are buried in the text and easy to
be missed if not paying close attention.

## Running `happy` in your project

Similar to `alex`, `stack` can run `happy` automatically on any file ending in
a `.y` extension. Add `happy` to your `cabal` file next to `alex` and you're
good to go!

As with the lexer, I added some simple test cases to the parser. I am keeping
this project in my GitLab account; see https://www.gitlab.com/gwasser/tigerc and
in particular `src/Tiger/Lexical/` for implementation details, and `test/` for a
full example on using `HUnit` and `Tasty`.

# Next time

We've started work on the lexer
and parser and reached a workable point, but before moving on we should ensure
that the lexer and parser are in fact parsing code correctly! I will look at a
better parser test suite, and consider how to write better grammar rules to
ensure Tiger is sufficiently covered.

[1]: https://en.wikipedia.org/wiki/Chomsky_hierarchy
[2]: https://www.haskell.org/happy/doc/html/
[3]: https://www.haskell.org/happy/doc/html/sec-sequences.html
