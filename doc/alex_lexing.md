Using Alex for simple lexing in Haskell
=======================================

The first stage of Tiger is the lexical analysis, that is,
reading the initial input a character at a time (or more generally, a byte or
two at a time depending on character encoding, etc.) and generating a stream of
*lexical tokens*. Lexical tokens basically "normalize" the input into a format
that makes further processing and analysis easier, so it makes sense to me why
you'd want to do this. But how exactly is this done?

# NFAs, DFAs, and regexes, oh my!

Chapter 2 of Appel's book starts with a short
but interesting discussion on *regular expressions* (REs), and how REs can be
turned into *finite automata* (FA). FAs seem to be the standard go-to for modeling
and creating lexers because of their relative simplicity.

FAs are state machines, machines that keep track of state as they process each
character (or byte) of input. We say that an FA *transitions* from one state to
the next upon reading each character of the input; if the FA runs out of input
to process, then the final state the FA is left in is used to determine the
token. If the final state is one that recognizes a given token, we call the
state an *accepting state*; otherwise, there is not match to a token, and the
input is rejected. Non-deterministic finite automata (NFAs) are the general form
of FAs that allow for the possibility of overlapping state transitions in which
a single input could result in one of multiple possible transitions (and
therefore more than one possible token match, hence the non-deterministic
aspect). However, there are theorems and algorithms that allow one to convert
any NFA into an equivalent deterministic finite automaton (DFA) which only
allows one possible transition for any given state and input. In principle DFAs
are easier to implement in software than NFAs because of the determinism, but
can be larger and take up more memory or time to run. Appel provides algorithms
that allow one to convert REs to NFAs and then to DFAs, making it easy to
express lexical tokens as REs and then generate a DFA that recognizes those
lexical tokens and can be implemented in software. In other words, REs are more
human-readable specifications, but DFAs are machine-readable.

OK, great, so I understand the principle here. We write REs that represent all
of the possible tokens we imagine for our programming language -- keywords,
literal types like strings and numbers, identifiers like variable or function
names -- and then translate that human-readable RE specification into an
equivalent DFA that is easy to implement as a program. How exactly is that done
in a programming language like ML or Haskell?

It is this point where Appel basically gives up and says approximately "It's
tedious writing your own lexer so just use a lexer generator like ML-Lex". After
all the build up of how easy DFAs are to understand, it seems disappointing a
simple example of one in real programming code isn't provided (even if it isn't
the full one for Tiger, just one for a simple example). There's diagrams of how
a DFA executes for a simple language, but not actual ML code showing how to
model a DFA and execute it. What's interesting is that the example Java code
given in Aho's Chapter 2 (or Appendix A, which the chapter refers to) doesn't
use DFAs either, at least not directly; Aho also provides simple definitions of
DFAs but the example code seems to just scan and select one token at a time in a
very naive way, there's no evidence a DFA was used (and in fact, as is typical,
the more complex features of the implementation are left as an exercise the
reader, so even if it is using a DFA in some hidden way I'm not picking up, it's
still not illustrating how to do more advanced techniques). Now I'm getting
suspicious. Can I search for other example implementations?

## Solved Problems

The first quote you will encounter (and, really, the only quote) once you start
looking for details of writing your own lexer is "it's a solved problem, use a
generator". Not very helpful advice for someone that wants to learn how to solve
the problem. Pages and pages of Google searches and forum posts contain this
quote, as if the problem is quite trivial, and yet almost no details on how to
actually do it.

I learned from graduate school in mathematics that the "trivial" problems in the
textbook missing associated proofs are often the most difficult (while sometimes
the proof is indeed short or obvious, many times the proof is only obvious the
expert practitioner that has worked in the field for years and memorized all of
the relevant definitions and lemmas that go into the "trivial" proof; and in
many cases I'm not even sure it was obvious to an expert). Inability to find
decent tutorials -- or being told that the details are left to the reader to
work out -- makes me immediately suspicious on its "ease".

## Haskell DFAs

Aside from the fact that it is difficult to find example lexers in general, a
functional implementation rather than an imperative one is even harder to find.
In particular, while ML is a functional language, it does also support
imperative programming, so the example algorithms for DFA generation given by
Appel are in imperative format, and Aho's examples are of course in imperative
Java as well. The recommendation from most academics is that functional
programming makes compiler implementation simpler, and yet all examples are in
imperative style!

I did come across one simple [Haskell implementation of DFAs][1] by Daniele
Micciancio, however. It's a simple straight-forward implementation that is easy
to run on its own; I like that it is pretty much exactly the mathematical
definition turned into code, which is an aspect of Haskell I really appreciate
but am still not very used to.

Running Micciancio's DFA implementation on some sample input quickly shows
problems, however; it requires that the entire input be used before it can
verify if an input is accepted or not. This means this DFA would attempt to read
an entire input as one token, which is of course not what we want. We want the
DFA to break a string down into its component tokens, not read the whole thing
as one token! If we knew how to break up the input into tokens to verify each
individually, then we wouldn't need to verify would we (we would already know
what the tokens are!). Aside from this, it also doesn't track things like
position in the input stream, so we wouldn't be able to print out helpful error
messages that pinpoint the problem. Without a separate stack of
already-processed characters, we can't do things like backtrack or error-correct
either.

Therefore this DFA implementation needs some modification to support reading a
stream of multiple tokens and not simply verifying a single token input. I
played around with it a bit, trying a few different ways to add in this extra
information to the processing but always ended up coding myself into corners. I
completely acknowledge my inexperience with both lexers and Haskell, so it's
entirely possible an expert could have done so, but I decided to put away the
idea of writing my own lexer for the time being. I can definitely conclude that
implementing lexing is, as one would expect, not as simple as the mathematics
lead you to believe! There's a lot of non-obvious bookkeeping in a real
implementation, which is probably why the books seem to shy away from it. Of
course, that's exactly what irritates me! I believe books should be documenting
this complicated knowledge in an accessible way so that we can teach future
generations; every time I tackle a subject like this, I grow more worried we're
moving toward a technological dark ages where the knowledge of the older Baby
Boomer and Gen X generations that started the computer revolution is lost on the
younger generations because nobody wanted to write it down and teach the young
the details.

# Using `alex` to generate a lexer

So, settling on using a lexer generator, how does it work in Haskell? The
standard lexer generator equivalent to `lex` or `flex` for Haskell is `alex`.

`alex` seems pretty straight-forward; as mentioned in the theory section above,
you are mostly just writing regular expressions for the kinds of tokens you want
to identify, and let `alex` handle generating the DFA structure. So a quick
refresher on regular expressions.

## Regular Expressions as Lexing Language

My first goal is to determine how to describe all of the possible tokens in the
Tiger language. Tokens break down into only a few possible types when you remove
whitespace: tokens are basically strings of text, or numbers. The main decision
is what "special" characters (such as underscores) your strings are allowed to
contain, and what types of numbers you expect to use (integer vs real being the
main distinction).

We generally use Regular Expressions (REs) to define the format of tokens and
what characters are allowed or not allowed. REs are pretty simple to follow:

* a character on its own in a RE means that character is required to be in
  exactly that position to match. We can therefore use the RE 'abc' to match
  exactly the phrase 'abc' and nothing else.
* if it is ok to have one of multiple possibilities for a character in a given
  position in a word, we use a character set by putting all of the acceptable
  characters into square brackets. '[abc]' means it is ok for a single character
  to be an 'a', a 'b', or a 'c'. So a 'd' would not match this RE, but a 'b'
  would. So you don't need to write out every letter of the alphabet, you can
  hyphenate, such as '[a-z]' which means "any lower case letter in alphabet
  would match this".
* The star '*' means "zero or more repetitions" of the RE before it. 'a*'
  means it can match any number of 'a's in a row (including no 'a's, just an
  empty input).
* A dot '.' means "match any character whatsoever". So could be a letter,
  digit, $, &, or anything else on the keyboard. So the combination '.*' is
  interpreted as "zero or more (the '*') of any character (the '.')", or
  basically, a string of any size.

You can [read on Wikipedia][2] for more information if you're not familiar with REs.
For Tiger, identifiers and numbers (integers only) are pretty simple REs:

    identifiers = [a-zA-Z][a-zA-Z0-9_]* numbers = [0-9][0-9]*

In other words, identifiers must start with a letter (lower or upper-case is
fine), but then can contain any number of letters, digits, or underscores after
that first letter. Numbers must start with a digit, followed by any number of
digits.

However, there is a problem with identifiers, and it's not even the RE's fault.
What if we use the identifier "if" as a variable name in a formula? The lexer
cannot just by reading characters tell if it is meant to be an identifier (a
variable or function name), or the keyword "if" that signifies the beginning of
an if-then-else statement. Some context is necessary here, which would require
our lexer to do more syntactic or semantic analysis. We prefer to avoid such
analysis until later stages; for now, we are only trying to perform lexical
analysis enough to generate tokens for later analysis.

Both Appel and Aho suggest one way to handle this issue is to simply declare
your keywords as "reserved", meaning one cannot use keywords as identifiers in
your programming language. I took their advice and declared all of the keywords
for Tiger as their own tokens, with their own simple regular expressions
(namely, simply the concatenation of the characters making up the keyword). When
processing an input, we use the rule that we attempt to match the keyword REs
first, to ensure that we always recognize input as keyword tokens rather than
general identifier tokens.

The reserved keywords in Tiger are:

    ARRAY | BREAK | DO | ELSE | END | FOR | FUNCTION | IF | IN | LET | NIL | OF
    | TO | VAR | THEN | TYPE | WHILE

Naively, strings seem like any number of any characters in between quotes:

    strings = \".*\"

But it turns out this is incorrect. In my testing, two separate string literals
were run together into one big string by the lexer, and it turned out it was
because of quote escaping within the input string. StackOverflow had a good
discussion, and a better regex for recognizing string literals is:

    strings = \"([^\\\"]|\\.)*\"

Strings are a quotation mark, followed by zero or more of either an escaped
character or a  character that is not the escape backspace or a quote,  ended by
another quotation mark. Makes sense to me.

Lastly, we haven't said anything about operations such as addition ("+"). Every
operator also gets its own regular expression, and its own token. For example,
the "+" input should be recognized as a `PLUS` token. At this stage we're not
checking if the operator is used correctly (for example, between two integers),
but only marking that we have seen this token in the input stream. Similarly, we
also need tokens for all types of parentheses and punctuation.

## Using `alex` within a `stack` project

The `stack` build tool makes it [easy to create a new Haskell project][3], and
build and test it. You can make a new project with the `stack new`
command. Just edit the `.cabal` file. I left the default set up of a separate `app/`
and `src/` folder, since I envision the same compiler library being used for a
compiler as well as an interpreter (two different apps), but you could use a
simpler directory layout too.

The `stack` build tool (really `cabal` underneath) makes it easy to work with `alex`.
In your `.cabal` file, simply add the line underneath your executable/library:

    build-tools:         alex

and now `stack` will automatically run `alex` on any file ending in a `.x` extension
to produce Haskell code, before continuing to compile the generated Haskell code
into the project. Pretty neat feature!

By default, `alex` creates a function called `alexScanTokens` that is the actual
lexer function. You need only import this function into other modules and call
it to process input and get the token stream. In the "simple" mode,
`alexScanTokens` returns a list of tokens of whatever type you wish to define. I
defined a new `Token` type with the tokens for the Tiger language, so
`alexScanTokens` returns a `[Token]` for me. I placed the `Token` definition in its
own `Tokens.hs` module and imported it into the `Lexer.x` file by placing this at
the top:

    {
    module Tiger.Lexical.Lexer (alexScanTokens) where
    import Tiger.Lexical.Tokens (Token(..))
    }
 
so that it could be easy for other parts of the compiler (e.g., the parser) to
import the definition and work with `Token` types. Add both the `Token`s and the
Lexer to the `.cabal` file's exposed-modules: section so they get processed. As I
said earlier, the `Lexer.hs` will automatically be generated from `Lexer.x` with
`alex` before compiling.

`alex` technically performs actions on each token as it is processed. The default
is to simply implement a lambda like `\s -> ARRAY` which means to take the
processed input string and throw it away and return an `ARRAY` token. However,
sometimes it can be useful to do some extra processing before returning a token.
For example, for identifying string literals, I used the line:

    \"([^\\\"]|\\.)*\"           { \s -> STR (init $ tail s) }

If the RE for a string literal on the left matches the current input, that input
is passed to a function defined on the right in curly brances which uses
`init $ tail` to strip the quotes from the string before returning it as a `STR`
token. We only want to save the actual literal string, not the quotes that
only delimit the string in a source code file.

This is all documented pretty well in the [Alex user manual][4].

To ensure my code was working correctly as I made changes, I used `HUnit` with the
`Test.Tasty` framework, which seems to be a common standard for unit testing. Test
cases are functions that look like:

    testLexerARRAY = testCase "accepts input 'array' as ARRAY" $
      assertEqual [] ([ARRAY]) (alexScanTokens "array")

where `assertEqual` takes as parameters a list you can ignore, the correct answer
(in this case an `ARRAY` token as the only token in the return token list), and
the function you want to test. I used a few of these including different tokens,
one character off from a token to ensure it counts as an identifier, numbers and
arithmetic, various white-spacing, nested comments, and string literals, which
is why I realized my original naive implementation of the RE for string literals
didn't work. You can easily run the tests with the stack test command.

The `Tasty` framework is documented in [its website][5]. In particular, check out the
full example on the main page.

I am keeping this project in my GitLab account; see
https://www.gitlab.com/gwasser/tigerc and in particular `src/Tiger/Lexical/` for
implementation details, and `test/` for a full example on using `HUnit` and `Tasty`.

I think this all worked out pretty well, though I am still disappointed to not
have my own "from scratch" lexer implementation. If I have time in the future, I
might revisit the topic.

# Continuing

I expect more blogs on the further stages of
compilers and code analysis. Next up is the parser!

I in no way present myself here as an expert in compilers, lexers, Haskell, or
other areas of computer science. What I hope is to document what I learn for
myself as a nice retrospective one day. In the meantime, to those who find this:
I hope that if you are also a beginner like me at compiler theory or Haskell,
that this and future blog posts may be of some use to you.

[1]: https://cseweb.ucsd.edu/classes/wi14/cse105-a/haskell/intro.html
[2]: https://en.wikipedia.org/wiki/Regular_expression#Basic_concepts
[3]: https://docs.haskellstack.org/en/stable/README/#quick-start-guide
[4]: https://www.haskell.org/alex/doc/html/index.html
[5]: http://documentup.com/feuerbach/tasty
