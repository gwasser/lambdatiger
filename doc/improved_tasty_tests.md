Improved Tasty HUnit unit tests in Haskell
==========================================

In this post I'll discuss improving testing for our `alex`-based lexer for the
Tiger language started in a previous post.

# Implementing unit tests in Haskell

A task I neglected a bit in the last few posts was unit testing, which is very
important particularly for tasks like parsing that can fail in all sorts of
non-intuitive ways.

Of course, we need good examples of correct code to feed in to perform good
tests. I tried to write a few tests including using different tokens, one
character off from a token to ensure it counts as an identifier, numbers and
arithmetic, various white-spacing, nested comments, and string literals. It is
more difficult to choose good test cases than it seems offhand. I found some
inspiration for test cases by looking at the Tiger language grammar and thinking
about corner cases.

However, it turns out Appel provides a number of sample Tiger programs in the
testcases folder in [Appel's starter code template][1], available on the book
website. So apart from the simple tests I already wrote in past posts, I
probably don't need to do much other than use Appel's tests.

## Tasty and Basic HUnit Testing

As for the testing harness code itself, I touched on this in a
previous post, but the idea is to use `HUnit` with the `Test.Tasty` framework,
which seems to be a common standard for unit testing. The basic way to use it
is the following:

1. Use `testCase` or a similar function to create an individual test, typically
   by using `assertEqual` to check if a function provides the answer you expect
2. Group `testCases` into a testing group using the `testGroup` function
3. Optionally, group multiple testing groups into a single group using
   `testGroup`
4. Run your `testGroup`, probably using `Tasty`'s `defaultMain` function

I wrote some tests and combined them into a lexer test group, some other tests
as a parser test group. Then combine the lexer test group and the parser test
group into a single test suite, which is then run using `defaultMain`. Actually
pretty simple. The `Tasty` framework is documented in [its website][2]. In
particular, check out the full example on the main page.

You can write simple test cases like the following:

    testLexerARRAY = testCase "accepts input 'array' as ARRAY"
      $ assertEqual [] ([ARRAY]) (alexScanTokens "array")

where `assertEqual :: String -> a -> a -> Assertion` takes a string that is used
as a prefix for testing messages (I ignore this by inserting the empty string or
list), the expected correct answer (in this case an `ARRAY` token as the only
token in the return token list), and the actual answer (which is the function
you want to test applied to appropriate input). Note that an `Assertion` is
actually just an `IO ()` action, which is key to understanding other ways to test.

As mentioned previously, Appel provides a large amount of example Tiger programs
for testing, so it makes sense to plug this into my testing framework. At first
I tried to copy and paste the code into my testing framework, but (1) this would
be a lot of manual work, and (2) Haskell doesn't have easy multi-line strings,
so I'd have to manually edit each example to use as a `String` input. It therefore
makes more sense to try to read directly from each file into a `String`, and then
apply the functions on them.

The basic way to read from a file is the Haskell function `readFile` but it
returns an `IO String` rather than a plain `String`. Therefore you can't use the
above test case directly because it would always fail trying to compare `[Token]`
versus `IO [Token]`.

## `IO` and Monads

Monads are one of those topics that sounds
imposing to many people, and this perception is made worse by mathematics geeks
such as myself getting exciting about the connections between monads and higher
level mathematics. Suffice it to say, one does not need to be a mathematician to
work with monads since the concept is actually pretty simple.

Monads can be looked at as "type wrappers" that provide *context* to the function
and ensure certain properties are true. The monad wrapper is basically providing
extra information about the value it is holding; for example, `IO` is a monad that
wraps data that is read into the program as input from the "world", and so the
difference between a `String` and an `IO String` is pretty clear. `IO` provides some
extra context that this is no ordinary `String`, but specifically one that has
come in from the outside world as input and might need special handling.

The most useful property of monads is that monadic functions (ones that return a
monad value) can be chained together, allowing data to be passed from the output
of one function directly to the input of another function in a chain. This turns
out a super useful property, so it's good to understand that concept. There's a
[blog post with pictures][3] on how monads work that is fantastic for visualizing
the process.

## `HUnit` Testing when reading from files

So circling back to our problem with plugging `IO` monad data into our lexer
functions that expect non-`IO` data, we can use properties of monads to make it
work. For example:

    testLexAppel15 = testCase "lexer accepts test15 (read from file)" $ (readFile
    "test/appeltestcases/test15.tig" >>= (assertEqual [] ([IF, NUM 20, THEN, NUM 3,
    TEOF])) . alexMonadScanTokens)

There's a few things going on in this example:

1. We're reading in from the file as an `IO String` which is therefore an
   instance of the `IO` monad and allows us to do things like apply bind. So
   we're reading in the text, unwrapping it from the `IO` monad using >>= (bind)
   and then handing it off to the next function for processing.
2. The next function is actually a function composition between `assertEqual`
   and `alexMonadScanTokens`.
3. By partially applying `assertEqual` to the prefix text `[]` and the expected
   answer (of type `[Token]`), we are taking advantage of currying to create a
   new function that has type signature `[Token] -> Assertion`. Luckily for us,
   `alexMonadScanTokens` provides that `[Token]` from the input `String` it
   received from the `>>=`.
4. Therefore the whole composed function takes a `String` (the original input
   directly from the file) and returns an `IO ()` (the action representing
   whether the lexer is correct), which is exactly the expected argument of
   `testCase` since `Assertion` is an alias for `IO ()`. The `$` operator is
   the function application operator with an extremely low precedence to ensure
   the functions on the right-hand side complete before being applied as an
   argument to the function on the left (it's really just the Haskell-y way of
   avoiding writing parentheses around things). We have therefore built a
   complete test case.

While I don't think this was an amazing "discovery", this sort of usage of
monads and types comes a bit with the territory of Haskell and I'm not sure is
well documented in examples, so it might be difficult for a beginner to think
about. I puzzled over it a little before I realized this. The general trick
seems to be to consider the types of the functions as a guide for how to plug
things together.

We could wrap up our two unit tests from this section into a test group by doing
something like the following:

    lexerTests = testGroup "Some lexer tests for Tiger" [testLexerARRAY,
    testLexAppel15]

Then in our main Haskell test module, we write:

    module Main where

    import Test.Tasty (defaultMain)

    main = defaultMain lexerTests

When ready, you can easily run the tests with the `stack test` command after you
make sure `tasty` and `tasty-hunit` are in the `build-depends` section of your
`cabal` file. The `cabal` file and basic test harness should have been created
when using `stack` to create a new project, but you can also check my GitHub
repo for a full example.

# What next

Aside from learning better practices (unit and regression
testing are important!), this exercise paid off because I learned that my lexer
implementation was in fact deficient. In the next post, I'll write about what I
needed to do to fix it.

[1]: https://www.cs.princeton.edu/%7Eappel/modern/ml/project.html
[2]: http://documentup.com/feuerbach/tasty
[3]: http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html
