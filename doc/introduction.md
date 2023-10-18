Welcome
=======
 
## What is this?

This page contains notes on the implementation of a compiler and interpreter
for the Tiger programming language.

The notes are written in the format of a tutorial, both to document my
thinking and what I learn, and also hopefully to help others learn.

This implementation attempts to use a pure Haskell interpretation, which
requires a full pure, lazy, functional implementation that you don't
really see anywhere else. It is interesting to see what differences that
makes compared with other implementation languages.

Tiger itself is small enough to form a simple core that can be extended
to object-oriented and functional language features. My personal goal
is to implement a functional language *a la* Haskell itself.

## Where?

The implementation is hosted within GitHub in the same repository as
this source for this tutorial book. My hope is that makes it easier to
find.

Check out the code as you go through the tutorial, or try to write it
yourself based on the tutorial's contents.

## Why?

Coming from a physics and mathematics background, I'm still playing catch-up on
some of the "software engineering" aspects of working in the infosec/CS space.
Obviously the mathematics background has helped make it easy to digest theory,
but turning that theory into real-world programs and software is still a
challenge for me. And honestly I suspect such work is still a challenge for a
lot of people, based on comments I keep reading.

Automation is effectively the goal. Humans have imperfect judgment and can make
errors. If nothing else, human labor doesn't effectively scale with the amount
of data flooding the internet. The interesting problems are therefore the ones
that may be automated. Since a lot of security work these days is finding or
detecting security issues in software so they may be fixed, it naturally makes
sense to wonder how much can be automated; this application is known broadly and
roughly as *code analysis*. It's a bit of a broad loaded term, but I think is
fair to apply.

Code analysis requires *a computer program that can digest other programs* as
input and do some analysis on them without any human input. If we take the
program's source code (say, C, C++, or Haskell source code) as input, we're
doing *source code analysis*, and if we take the program's compiled executable
bits (say, IA-32 x86 machine code) as input, we're doing *binary code analysis*.
In either case, this basically means that, all by itself, the code analysis
program must be able to read another program, determine its structure and
meaning (syntax and semantics), and from there, infer security properties of the
input program or actions that a human analyst needs to take. This has a
(possibly not too surprising) lot in common with compilers, programs that read
source code to translate into another language. We're effectively calling for a
compiler that translates program code (whether source or binary) into some
*security-focused language*, that makes security risks and vulnerabilities clear
so that the issue may be corrected automatically or a human analyst informed to
take action otherwise. Where the traditional compiler has several passes for
optimization purposes, code analysis requires several passes to better
understand the security semantics of the code. The architecture of the two is
very similar, and so it makes sense to me to learn more about how compilers
function as a way to improve my code analysis skills.

For that reason, I decided to get some books and do a little self-study. The two
that popped out to me (in particular, recommended as part of Carnegie Mellon's
compilers course) are the "dragon book"
(*Compilers: Principles, Techniques and Tools*, by Alfred V Aho, et. al.) and
*Modern Compiler Implementation in ML*, by Andrew W. Appel. Appel's book is
meant as a textbook for a more project-focused
course, so I decided I'd follow that and refer to the dragon book as necessary.
Appel's book teaches you to implement "Tiger", a small programming language with
a somewhat ML-y syntax. My goal is to get through the main part of the book and
implement Tiger over the next couple months. Seeing the growth of functional
languages recently (Scala, Rust, etc.), I decided to take the advice of most
sources and stick with a functional language as the implementation language. I
specifically chose Haskell to force myself to learn functional paradigms and not
use imperative ones as a crutch; Haskell also appeals to my mathematician side
as it is a very elegant language, and it is seeing increased use in industry
such as at Facebook, whereas other more proper functional languages such as ML
still seem to be completely trapped in academia.
