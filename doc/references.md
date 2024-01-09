 
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
* Panne, Sven. "Modern Compiler Implementation in Haskell", porting of Appel's ML examples into Haskell. <https://github.com/svenpanne/tiger>.

A larger list that I haven't read much yet is [Awesome Compilers](https://github.com/aalhour/awesome-compilers), but I'm putting it here to help myself and others.

License
-------

This software and accompanying documentation are Copyright (C) 2016-2018 Garret Wassermann.

This software distribution (including any documentation) is licensed under the GNU General Public License version 3. Please see the included COPYING file with this distribution.
