Handling strings and comments with Alex and monads in Haskell
=============================================================

In this post I'll discuss improving our `alex`-based lexer due to some unit
tests I implemented that made me realize something was going horribly wrong with
our original naive lexer rules.

# Discovering Lexing Problems

Last post, I used a suite of example Tiger programs written by the textbook
author to build a testing system for the lexer and parser stages of my compiler.
The easiest way to implement test cases, particularly for larger programs,
seemed to be to run it thru the lexer, manually examine output, and see if it
matches expectations. If so, copy and paste it into the testing code. In this
way, we're protected from regression bugs in the future while also checking our
code works as expected right now. As I began implementing test cases for my
parser, I realized that some of the examples (that should definitely work, since
many were example programs from the textbook!) were not parsing properly.

Because of the error message text (something to do with lexing newlines and
tabs), initially it seemed like the result might be some type of character
encoding. Verifying that my lexer routines seemed to handle whitespace correctly
(by using the predefined whitespace command), I was a bit puzzled. Removing some
of the whitespace and comments resulted in a successful parse. Well that's odd -
why isn't it properly parsing the whitespace and comments? Especially when it
*does* work in other test cases?

After some more investigation, the problem became apparent: the parse seemed
unsuccessful in cases that had two or more comments. I tweaked some test cases
until I finally discovered the root of the issue: my lexer rule was, as
expected, using a "longest match" rule, and so when two or more comments existed
in the same source, everything from the first open comment token (`/*`) to the
last comment's closing symbol (`*/`) was being stripped. That is, the two comments
were being seen by the lexer as a single very long comment, meaning any code in
between was considered part of the comment and stripped!

# Parser theory revisited

In hindsight, I shouldn't have been surprised at this; obviously, the longest
match rule is a well-known thing. The wording of the Tiger specification in
Appel's book should have also been a clue, for it includes only a short sentence
on comments, but specifically says the comments need an opening and closing tag,
and can be *nested*. For reasons we discussed in the previous post on parsing,
nested structures can only be successfully recognized by more powerful pushdown
automata in parsing; the simple finite automata used in lexing are insufficient
to recognize them. We must conclude that Tiger's comments cannot actually be
handled by a simple lexer alone, and the regular expression I was using to match
comments didn't actually work according to specification (and furthermore, no
regular expression could *ever* work!).

Therefore I seem to have two options here. The first option is to only recognize
the comments open and close tags as tokens, and include comments as part of the
token stream for the parser to sort out and ignore (after verifying that every
nested comment is properly balanced, much like we would for parentheses). This
would work and makes sense on some level since checking for balance is squarely
a power of parsers. However, it offhand feels like it muddies the waters a
little between the job of the lexer and the job of the parser; in my mind, the
parser is structuring already validated data, and it is up to the lexer to
determine if that input is even valid in the first place. In any case, we would
need to modify both the lexer and parser to work in this scenario.

The second option is to modify the lexer only, so that it can find the comments
first and filter them out of the token string, but without stripping valid data
as our test cases show our initial implementation did. At this point, the lexer
is not strictly-speaking a finite automaton anymore, since adding the ability to
check balance and track state is not a property of regular expressions, but we
hope that this minor modification is worthwhile to keep the rest of our lexer
and parser running fast. In other words, we let `alex` optimize the bulk of the
lexer code, and only use the slower balancing code when it detects the start of
a comment. By keeping such code in our lexer, it retains the logical separation
of tasks, and means that our parser does not even need to be updated.

This second option therefore seems best for my circumstance, but either will
technically work. I will proceed with the option of updating the lexer to
properly handle comments and strings before passing the token string on to the
parser. In fact, this is such a common situation that we will find that `alex`
has built-in features to handle this problem.

## `alex` states and startcodes

The `alex` user manual mentions that start codes can be used to do things like
recognize strings. Essentially, start codes are labels for states that the
lexer tracks. At any given point in time, the lexer is running in a particular
state; for basic usage, the lexer stays in its initial state (labeled as
state 0) the entire time and never changes. However, by adding a state
identifier to the beginning of lexer rules and re-writing lexer actions to
suit our needs, we can cause the lexer to transition to new states as it
processes input. Only lexer rules specifically tagged for that state can fire;
all others will be ignored until the lexer transitions to a new state. In this
way, we can define a "string" state that allows us to ignore trying to match
tokens until we are finished processing the string and transition back to the
default state.

As you might have guessed, this description also applies equally well to
comments. We simply create a "comment" state for the lexer that also ignores
most token rules until the end of comment token is processed and the lexer
returns to its default state.

Comments are a little more special than string literals, however, because of our
specification that comments can be nested. In other words, we need some way of
tracking how many begin-of-comment tokens we've seen, and ensuring we've seen
exactly that many end-of-comment tokens to close out the comment before we
return the lexer to the default state to resume processing standard tokens.
Functional programming however does not have typical means of storing state
though; for functional programs, "state" is maintained by simply passing
information around between functions and actions. We basically need to chain
functions together in order to generate our initial state, update the state
based on current input, and return the new state for the next input symbol to be
processed. In Haskell, whenever you think of "chaining" values between
functions, you should immediately start thinking of `monads`. So, like our
testing harness earlier, we turn to some monad concepts to finish our lexer
implementation.

# A monadic lexer using `alex`

As we process input one character (or byte!) at a time, we need to keep track
of state so we can recognize string
literals and comments. We do this in a functional language like Haskell by
packaging that state data into a structure, and outputting the current state
after every character we process. As we process the next character, that state
is fed in as an input, updated if necessary, and returned as output for the
next. As we said earlier, this forms a chain of output to input, and that is
best represented by a monad. In fact, it is such a common pattern that Haskell
has a built-in monad type called `State` for doing just this in an easy-to-use
fashion.

To get some inspiration on how to use State monads, I looked around for sample
implementations. A [StackOverflow thread][1] gave a simple
[monadic implementation in `alex`][2], but perhaps a little more clear to
follow was the discussion in [the blog][3] for a short (unfinished) book
["Alex and Happy"][4]. Both examples are incomplete as
you can imagine, and designed for very small example languages; I had to expand
them to work with a full language like Tiger.

# `alex` wrappers

We previously were using `alex`'s basic wrapper, that handled a lot of the work
for us and automatically generated a function that returns a list of `Token`.
However, we're not really allowed to customize this function much and so can't
get `State` monads into it. Luckily, alex provides a specific monad wrapper for
exactly this purpose, that allows you to write your own code for more custom
processing of the input. It seemed like it might be sufficient, but it does
require you to use the specific auto-generated `alex` API which still may be
insufficient for some use cases. Therefore, I followed the example of
"Alex and Happy" and used `alex`'s [low-level API][5] to get more control over
tracking state and reading input.

Essentially, to use `alex` in this manner, you get to define the following
three things:

    type AlexInput
    alexGetByte       :: AlexInput -> Maybe (Word8,AlexInput)
    alexInputPrevChar :: AlexInput -> Char

And in return, `alex` will automatically generate an `alexScan` function for you
that reads in the next `Token`. Pretty nifty!  You can call `alexSca`n from your own
function and then handle the return data as desired. In our case, we want to
define `AlexInput` and our own monad that we call `LexerState` (based on the `State`
monad) to track things we need. Specifically, `LexerState` will track whether we
are currently in the "comment" or "string" state, or the default lexer state, as
well as provide a buffer to hold the string literal as we process it or to track
the number of start-of-comment tokens we have seen (so we can verify balance).

We then define some lexer actions that do more than simply return the `Token` type
matched; they will update the `LexerState` as needed when we transition from
default mode to reading a comment, etc. They look like the following:

    beginComment :: LexAction
    beginComment _ _ = do
      s <- get
      put s {lexSC = commentSC,
            commentDepth = (commentDepth s)+1}
      return Nothing

The two parameters of `beginComment` come from currying on the definition of
`LexAction`. We're taking in the `LexerState` and using the pre-defined `get` function
(defined for `State` monad) to get the state, then we put new data into the state
(we're updating the `lexSC` -- lexer startcode -- to transition to the `commentSC`
state, and we're adding 1 to the current count of begin-comment tokens so we can
validate later that we closed the right number of comments) using the
pre-defined put function. We finally return a `Nothing` indicating that no `Token`
was matched yet, we just updated some state.

There's more functions like `endComment` (we subtract 1 from the current
begin-comment token count, and if it goes to 0, we also return the lexer to the
default state) and ones for tracking string literal state and handling escape
codes in strings. Since they are pretty straight forward, I will omit them here
and refer you to my code repository for the remaining details.

The upshot here is we now have a lexer that can properly handle nested comments
and strings. Running the new lexer on my previously failing test cases with
nested comments, etc., shows that the lexer performs its job correctly (at least
on the test cases I have).

# An Aside on Lexing

A quick aside shows that this problem with correctly lexing
and parsing strings and comments is likely more widely spread than we're aware.
As I was looking at the sample code from "Alex and Happy" on GitHub, I spotted
the following:

[:image ]

The open comment tag is inside of quotes and should be treated as a string, but
the syntax highlighting missed the string literal and so displays all of the
text until the next comment tag in faded gray. In other words, the syntax
highlighter thinks there's an actual comment here because it isn't parsing the
string literals correctly! This is exactly the kind of problem we solved in this
post using monads and `alex`.

# Up Next

We need to review our `happy`-based parser and possibly update it to use
monads as well. In any case, with a better lexer in the pipeline, we can expand
our unit testing to ensure the compiler front end is doing what we think it is.

[1]: https://stackoverflow.com/questions/24709175/how-to-parse-c-style-comments-with-alex-lexer
[2]: http://lpaste.net/107377
[3]: https://www.jyotirmoy.net/posts/2015-08-17-alex-happy-startcodes.html
[4]: https://leanpub.com/alexandhappy
[5]: https://www.haskell.org/alex/doc/html/basic-api.html
