gwhc
====

GWass's Haskell Compiler library, with compiler and interpreter frontends

What?
-----

This is a small Haskell parsing, analysis and x86 assembly code generation library, with a compiler and interpreter front-end, all written in Haskell. I should also perhaps say "will be", not "is", since it is still a work in progress.

This is my work based on Stephen Diehl's "Write you a Haskell" so I can learn more about functional programming and compilers.

Consider this research quality, not production quality. This is more for my own learning, but I hope it is useful to you if you're interested. I hope to include documentation, comments, and proper git branches/tags to make it easy to study and learn from, as well as simply give an example of a well-written Haskell project.

Building
--------

This compiler is itself written in Haskell.

To compile, you ideally will have `ghc` and `stack`. Compile with

    stack build 

Then you can test the interpreter with

    stack test
    
You can run the command you prefer (`gwhc` the compiler, or `gwhi` the interpreter) with stack using

    stack exec gwhi
    
You can either redirect a file from stdin, or provide filename(s) as parameters for interpretation. You pass parameters to the actual executable and not stack by putting them after a `--`, such as `stack exec gwhi -- --help`.
    
References
----------

This code is 100% my own work, but concepts are based on those from several textbooks and sources. I have compiled (see what I did there?) a Recommended Reading List of references below that may be helpful if you're looking to create your own compiler.

* Diehl, Stephen. Write You a Haskell. <http://dev.stephendiehl.com/fun/>.
* Pfenning, Frank, et al, CMU 15-411 Lecture Notes. <https://www.cs.cmu.edu/~fp/courses/15411-f13/>
* Appel, Andrew W. Modern Compiler Implementation in ML.
* Aho, Alfred, et al. Compilers: Principles, Techniques, and Tools, 2nd Edition. (also known as "The Dragon Book")
* Ranta, Aarne. Implementing Programming Languages: An Introduction to Compilers and Interpreters.

A larger list that I haven't read much yet is [Awesome Compilers](https://github.com/aalhour/awesome-compilers), but I'm putting it here to help myself and others.

License
-------

This software and accompanying documentation are Copyright (C) 2016-2017 Garret Wassermann.

This software is licensed under the GNU General Public License version 3. Please see the included LICENSE file with this distribution.
