Starting A New Haskell Project With Stack
=========================================

Before we can create a compiler, we of course have to setup the Haskell toolchain and start a new project. There are some best practices for setting up a new project and understanding the recommended project layout, so let's go over that quick here.

Feel free to skip to the next chapter if you're already familiar with the Haskell toolchain, including `stack` and `cabal`.

# Downloading Haskell

The new recommended way to install Haskell is using `ghcup`.

I am using Linux; there may be extra complications if you are using Windows.

You can use `ghcup` to install all the typical tools; I am using most of the latest versions of all tools except for GHC itself. I find that the latest GHC is not well supported by `cabal` and `stack` yet, so I am currently using GHC 9.6.x (using the latest bugfix of this series). This is reflected in the `stack.yml` file which relies on Stackage LTS.

# Creating A New Stack Project

Once your Haskell environment is set up, run:

    stack new tiger

To create a new Haskell project using the default template. The new project will be named `tiger` but you can change the name to whatever you prefer.

The default layout is:
- `app/`
- `lib/`
- `test/`
- `stack.yml`
- `tiger.cabal`
- `Setup.hs`

By default, each file in `app/` will generate its own executable. We can eventually place a separate `Compiler.hs` and `Interpreter.hs` command in here that share the same libraries. The libraries of course go in `lib/`. Testing files such as unit tests go in `test/`.
