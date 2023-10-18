lambdatiger
===========

Tiger programming language implementation, with compiler and interpreter frontends

What?
-----

Tiger is a small programming language defined in _Modern Compiler Implementation in ML_ by Andrew Appel (Cambridge University Press, 2004). The base Tiger language consists of nested functions, record values with implicit pointers, integer and string variables, arrays, and simple imperative language control constructs. It can be extended with object-oriented and functional features.

`tigerc` is a compiler for the Tiger language. I also include an interpreter called `tigeri`. Both executables utilize a shared library of features for lexing, parsing, analysis, and assembly code generation.

Consider this experimental research quality, not production quality. This is more for my own learning, but I hope it is useful to you if you're interested. I hope to include documentation, comments, and proper git branches/tags to make it easy to study and learn from, as well as simply give an example of a well-written Haskell project.

Also, consider this a work-in-progress, as many of the features described above do not exist yet.

Building
--------

This compiler is itself written in Haskell. Aside from standard `ghc` compiler libraries, you will need `tasty` testing framework installed. To compile, you ideally will use `stack` since it can manage dependencies and the build.

Compile with

    stack build 

Then you can test the compiler library with

    stack test
    
You can run the command you prefer (`tigerc` the compiler, or `tigeri` the interpreter) with stack using

    stack exec (COMMAND_NAME)
    
You can either redirect a file from `stdin`, or provide filename(s) as parameters for interpretation. You can pass parameters to the actual executable and not `stack` by putting them after a `--`, such as `stack exec tigerc -- --help`.

Tutorial
--------

I am maintaining a tutorial set of notes about the implementation as an [mdBook](https://rust-lang.github.io/mdBook/index.html) for easy online viewing. The source for the tutorial is included in this repository.

The tutorial can be viewed online at [gwasser.github.io/lambdatiger](http://gwasser.github.io/lambdatiger). It is automatically built using the GitHub Actions pipelines.

To build the book manually, you will need a Rust toolchain installed with the `mdbook` command install. If you don't have the `mdbook` command available, install it with:
```
cargo install mdbook
```
I also use the [`mdbook-pdf`](https://crates.io/crates/mdbook-pdf) extension to build to a PDF for printing and publishing, install it with:
```
cargo install mdbook-pdf
```

Once you have the prerequisite commands installed, run:

```
mdbook build
```

You can also serve and view in web browser for easier viewing and automatic updates:

```
mdbook serve
```
By default, the files are served at `localhost:3000`.
    
References
----------

The Tiger language and overall design of the compiler is based on the Tiger language and compiler presented in the book:

* Appel, Andrew W. _Modern Compiler Implementation in ML_. Cambridge University Press, 2004.

However, the book implements sample code in ML, and does not provide sample code for all details of the compiler.
The `tigerc` project is an attempt at a Haskell implementation of a similar Tiger language compiler. I in particular intend on extending the Tiger language with functional features, as suggested later in the book.

Aside from the main text, the following may be useful:

* Dornan, Chris, et al. _Alex User Guide_. <https://www.haskell.org/alex/doc/html/index.html>.
* Marlow, Simon, et al. _Happy User Guide_. <https://www.haskell.org/happy/doc/html/>.
* Bhattacharya, Jyotirmoy. _Alex and Happy: Lexers and Parsers in Haskell_. Lean Publishing, 2015. <https://leanpub.com/alexandhappy>.
* Aho, Alfred, et al. _Compilers: Principles, Techniques, and Tools_ (also known as "The Dragon Book"). 2nd Edition. Pearson Education, 2007.
* Ranta, Aarne. _Implementing Programming Languages: An Introduction to Compilers and Interpreters_. College Publications, 2012.
* Pfenning, Frank, et al. _CMU 15-411 Lecture Notes_. <https://www.cs.cmu.edu/~fp/courses/15411-f13/>.
* Diehl, Stephen. _Write You a Haskell_. <http://dev.stephendiehl.com/fun/>.

A larger list that I haven't read much yet is [Awesome Compilers](https://github.com/aalhour/awesome-compilers), but I'm putting it here to help myself and others.

License
-------

This software and accompanying documentation are Copyright (C) 2016-2023 Garret Wassermann.

This software distribution (including any documentation) is licensed under the GNU General Public License version 3. Please see the included COPYING file with this distribution.
